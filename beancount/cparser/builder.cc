#include "beancount/cparser/builder.h"
#include "beancount/ccore/std_utils.h"

#include <filesystem>

#include "absl/status/status.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_split.h"
#include "google/protobuf/descriptor.pb.h"
#include "google/protobuf/text_format.h"
#include "re2/re2.h"

namespace beancount {
namespace parser {
namespace filesystem = std::filesystem;
using absl::StrFormat;
using google::protobuf::FieldDescriptor;
using google::protobuf::TextFormat;
using std::string;

Builder::Builder(scanner::Scanner& scanner) :
  scanner_(scanner)
{
  info_.reset(new options::ProcessingInfo());
  options_.reset(new options::Options());
  Initialize();
}

Builder::~Builder() {
  for (auto* directive : directives_) delete directive;
  for (auto* error : errors_) delete error;
}

// TODO(blais): Review and remove this, this will not be needed once we process
// all the numbers in a second processing step.
void Builder::Initialize() {
  // Set Decimal context before processing, update the desired precision for
  // arithmetic operations.
  //
  // Note: Review this, perhaps allow the user to override it (would require
  // post-poning the computation of arithmetic expressions).
  context_ = decimal::context;
  context_.prec(28);
}

void Builder::AddOptionBinary(const string& key, string&& value, const location& loc) {
  string value_str = std::move(value);

  // Translate legacy option names to proto equivalents.
  static std::vector<std::pair<string, string>> translations = {
    {"name_assets",       "roots { assets: '%s' }"},
    {"name_liabilities",  "roots { liabilities: '%s' }"},
    {"name_income",       "roots { income: '%s' }"},
    {"name_expenses",     "roots { expenses: '%s' }"},
    {"name_equity",       "roots { equity: '%s' }"},
  };
  for (const auto& translation : translations) {
    if (key == translation.first) {
      value_str = absl::StrFormat(translation.second.c_str(), value_str);
      return AddOptionUnary(value_str, loc);
    }
  }

  // Check the options field and get relevant descriptors.
  const auto* descriptor = options_->GetDescriptor();
  const auto* field = descriptor->FindFieldByName(key);
  if (!field) {
    AddError(StrFormat("Invalid option: '%s'", key), loc);
    return;
  }

  // Issue a warning if the option is deprecated.
  if (field->options().deprecated()) {
    AddError(StrFormat("Option '%s' is deprecated; ignored.", key), loc);
    return;
  }

  // Preprocess fields which are intended for mappings.
  // This is essentially for backward compatibility for "inferred_tolerance_default".
  if (field->is_map()) {
    std::vector<string> components = absl::StrSplit(value_str, ":");
    if (components.size() != 2) {
      AddError(StrFormat("Invalid format for '%s': '%s'", key, value_str), loc);
      return;
    }
    value_str = absl::StrFormat("{ key: '%s' value: '%s' }", components[0], components[1]);
  }


  // Set the field on the options proto.
  if (field->type() == FieldDescriptor::TYPE_STRING) {
    value_str = absl::StrCat("\"", absl::CEscape(value_str), "\"");
  }
  if (!TextFormat::ParseFieldValueFromString(value_str, field, options_.get())) {
    AddError(StrFormat("Could not parse and set option '%s' with value '%s'; ignored.",
                       key, value_str), loc);
    return;
  }
}

void Builder::AddOptionUnary(const string& proto_str, const location& loc) {
  if (!TextFormat::MergeFromString(proto_str, options_.get())) {
    AddError(StrFormat("Could not parse and merge options proto with value '%s'; ignored.",
                       proto_str), loc);
    return;
  }
}

void Builder::AddInclude(string&& filename) {
  info_->add_include(filename);
}

void Builder::AddPlugin(string&& name, const std::optional<string>& config) {
  auto* plugin = info_->add_plugin();
  plugin->set_name(name);
  if (config.has_value()) {
    plugin->set_config(std::move(config.value()));
  }
}

const string& Builder::InternAccount(string&& account, const location& loc) {
  auto iter = accounts_.insert(std::make_pair(account, loc));
  return (*iter.first).first;
}

string Builder::MakeAbsolutePath(const string& filename) {
  if (!filename.empty()) {
    filesystem::path document_path(filename);
    if (document_path.is_relative()) {
      const string* filename = scanner_.location().begin.filename;
      assert(filename);
      filesystem::path parser_filename(*filename);
      return parser_filename.parent_path() / document_path;
    }
  }
  return string(filename);
}


void Builder::SetTagsAndLinks(const TagsLinks* tags_links, Directive* message) const {
  if (!active_tags_.empty()) {
    // First copy the active tags.
    auto* output = message->mutable_tags();
    for (const auto& active_tag : active_tags_) {
      *output->Add() = active_tag;
    }
  }

  if (tags_links != nullptr) {
    // Add the new tags.
    if (!tags_links->tags.empty()) {
      // Avoiding duplicates with active metadata.
      for (const auto& tag : tags_links->tags) {
        if (active_tags_.find(tag) == active_tags_.end()) {
          *message->mutable_tags()->Add() = tag;
        }
      }
    }

    // Add the new links.
    if (!tags_links->links.empty()) {
      auto* output = message->mutable_links();
      for (const auto& link : tags_links->links) {
        *output->Add() = link;
      }
    }
  }
}

void Builder::PushTag(const string& tag) {
  active_tags_.insert(tag);
}

void Builder::PopTag(const string& tag, const location& loc) {
  auto iter = active_tags_.find(tag);
  if (iter != active_tags_.end()) {
    active_tags_.erase(iter);
  } else {
    AddError(StrFormat("Attempting to pop absent tag: '%s'", tag), loc);
  }
}

void Builder::PushMeta(std::string_view key, MetaValue* value) {
  auto& value_list = active_meta_[key];
  value_list.push_back(value);
}

void Builder::PopMeta(const string& key, const location& loc) {
  auto iter = active_meta_.find(key);
  if (iter == active_meta_.end()) {
    AddError(StrFormat("Attempting to pop absent metadata key: '%s'", key), loc);
  } else {
    auto& value_list = iter->second;
    assert(value_list.size() > 0);
    auto* value = value_list.back();
    value_list.pop_back();
    if (value_list.empty()) {
      active_meta_.erase(iter);
    }
    delete value;
  }
}

void Builder::AddActiveMetadata(Meta* meta, Directive* dir) const {
  // Append active metadata into the output.
  if (!active_meta_.empty()) {
    Meta* dirmeta = dir->mutable_meta();
    for (auto [key, value_list] : active_meta_) {
      auto* kv = dirmeta->mutable_kv()->Add();
      kv->set_key(key);
      kv->mutable_value()->CopyFrom(*value_list.back());
    }
  }

  // Merge given metadata on top of active metadttaa.
  if (meta != nullptr) {
    Meta* dirmeta = dir->mutable_meta();
    dirmeta->MergeFrom(*meta);
  }
}

void Builder::ValidateMetadata(const Meta* meta, const location& loc) {
  // Check for the presence of duplicate keys and log an error if found. (Note:
  // In order to support multi-dicts, we need for some way to be able to declare
  // the keys as such in order to create consistent data tyeps for them across
  // transactions.)
  std::set<string> keys;
  for (const auto& kv : meta->kv()) {
    const auto& key = kv.key();
    if (!key.empty() && !keys.insert(key).second) {
      AddError(StrFormat("Duplicate metadata key: '%s' with value '%s'",
                         key, kv.value().DebugString()), loc);
    }
  }
}

absl::Status Builder::MergeCost(const inter::CostSpec& new_cost_spec, inter::CostSpec* accumulator) {
  if (new_cost_spec.has_number_per() && accumulator->has_number_per()) {
    return absl::InvalidArgumentError("Duplicate `number_per` cost spec field.");
  }
  if (new_cost_spec.has_number_total() && accumulator->has_number_total()) {
    return absl::InvalidArgumentError("Duplicate `number_total` cost spec field.");
  }
  if (new_cost_spec.has_currency() && accumulator->has_currency()) {
    return absl::InvalidArgumentError("Duplicate `currency` cost spec field.");
  }
  if (new_cost_spec.has_date() && accumulator->has_date()) {
    return absl::InvalidArgumentError("Duplicate `date` cost spec field.");
  }
  if (new_cost_spec.has_label() && accumulator->has_label()) {
    return absl::InvalidArgumentError("Duplicate `label` cost spec field.");
  }
  if (new_cost_spec.has_merge_cost() && accumulator->has_merge_cost()) {
    return absl::InvalidArgumentError("Duplicate `merge_cost` cost spec field.");
  }

  accumulator->MergeFrom(new_cost_spec);
  return absl::OkStatus();
}


void Builder::WitnessDecimal(const decimal::Decimal& dec, const string& currency) {
  // TODO(blais): Update display context stats. See grammar.Builder.dcupdate().
}

Directive* Builder::MakeDirective(const absl::CivilDay& date,
                                  Meta** meta,
                                  TagsLinks** tags_links) const {
  auto* dir = new Directive();

  // Set the date.
  DateToProto(date, dir->mutable_date());

  // Copy the metadata, if provided, including the stack's metadata.
  Meta* ourmeta = nullptr;
  if (meta != nullptr) {
    ourmeta = *meta;
    meta = nullptr;
  }
  AddActiveMetadata(ourmeta, dir);
  delete ourmeta;

  // Update tags, including adding active tags, and links.
  if (tags_links != nullptr) {
    SetTagsAndLinks(*tags_links, dir);
    if (*tags_links != nullptr) {
      delete *tags_links;
      *tags_links = nullptr;
    }
  }

  return dir;
}

void Builder::AppendDirective(Directive* directive) {
  directives_.push_back(directive);
}

void Builder::PreparePosting(Posting* posting,
                             const Amount* units_spec,
                             const char flag,
                             const string& account,
                             bool is_total_price,
                             const location& loc) {
  assert(posting != nullptr);

  // Set the units spec on the posting, if present.
  if (units_spec != nullptr) {
    auto* spec = posting->mutable_spec()->mutable_units();
    if (units_spec->has_number()) {
      spec->mutable_number()->CopyFrom(units_spec->number());
    }
    if (units_spec->has_currency()) {
      spec->set_currency(units_spec->currency());
    }
  }

  // Store flag and account name.
  if (flag != '\0') {
    posting->set_flag(&flag, 1);
  }
  posting->set_account(account);

  // Check conditions renamed to price annotations.
  if (posting->has_spec() and posting->spec().has_price()) {
    auto& spec = posting->spec();
    const auto& price = spec.price();

    // If the price is specified for the entire amount, compute the effective
    // price here and forget about that detail of the input syntax.
    if (is_total_price && (!spec.has_units() || !spec.units().has_number())) {
      // units.number is MISSING.
      // Note: we could potentially do a better job and attempt to f
      // this up after interpolation, but this syntax is pretty rare
      // anyway.
      AddError(StrFormat("Total price on a posting without units: %s.",
                         price.DebugString()), loc);
    }

    // Note: Allow zero prices because we need them for round-trips for
    // conversion entries.
    //
    // if price is not None and price.number == ZERO:
    //     self.errors.append(
    //         ParserError(meta, "Price is zero: {}".format(price), None))

    // If both cost and price are specified, the currencies must match, or
    // that is an error.
    if (spec.has_cost() &&
        spec.cost().has_currency() &&
        price.has_currency() &&
        spec.cost().currency() != price.currency()) {
      AddError(StrFormat("Cost and price currencies must match: %s != %s",
                         posting->spec().cost().currency(), price.currency()), loc);
    }
  }
}

void Builder::FactorTotalPrice(inter::Spec* spec) {
  // If the price is specified for the entire amount, compute the effective
  // price here and forget about that detail of the input syntax.
  if (!spec->has_price()) {
    return;
  }
  auto* price = spec->mutable_price();
  bool is_total_price = price->is_total();
  price->clear_is_total();

  if (is_total_price) {
    if (!spec->has_units() || !spec->units().has_number()){
      // This is the error case; clear the price if it happens.
      spec->clear_price();
    }
  } else if (price->has_number()) {
    decimal::Decimal dunits = ProtoToDecimal(spec->units().number());
    decimal::Decimal dprice;
    if (dunits.iszero()) {
      dprice = dunits;
    } else {
      dprice = ProtoToDecimal(price->number()).div(dunits.abs(), context());
    }
    DecimalProto(dprice, price->mutable_number());
  }
}

void SetLocationFromLocation(const location& loc, Location* output) {
  if (loc.begin.filename) {
    output->set_filename(*loc.begin.filename);
  }
  output->set_lineno(loc.begin.line);
  output->set_lineno_end(loc.end.line);
}

void Builder::AddError(std::string_view message, const location& loc) {
  auto* error = new Error();
  errors_.push_back(error);
  error->set_message(Capitalize(message));
  SetLocationFromLocation(loc, error->mutable_location());
}

static const char* kAccCompNameRE = "[A-Z][A-Za-z0-9-]*"; // "[\p{Lu}\p{Nd}][\p{L}\p{Nd}\-]*";

// Set default values for account roots.
void SetAccountRootsDefaults(options::AccountRoots* roots) {
  if (roots->assets().empty()) roots->set_assets("Assets");
  if (roots->liabilities().empty()) roots->set_liabilities("Liabilities");
  if (roots->equity().empty()) roots->set_equity("Equity");
  if (roots->income().empty()) roots->set_income("Income");
  if (roots->expenses().empty()) roots->set_expenses("Expenses");
}

// Build up an account regular expression.
re2::RE2 BuildAccountRE(const options::AccountRoots& roots) {
  const string root_names_re = absl::StrJoin({
      roots.assets(),
      roots.liabilities(),
      roots.equity(),
      roots.income(),
      roots.expenses()}, "|");
  return re2::RE2(StrFormat("(%s)(:%s)+", root_names_re, kAccCompNameRE));
}

void Builder::ValidateAccountNames() {
  auto roots = options_->roots();
  SetAccountRootsDefaults(&roots);
  re2::RE2 account_re = BuildAccountRE(roots);
  for (const auto& item : accounts_) {
    if (!re2::RE2::FullMatch(item.first, account_re)) {
      AddError(StrFormat("Invalid account name: '%s'", item.first), item.second);
    }
  }
}

void Builder::Finalize(const location& loc) {
  // Validate the account names, issuing errors as a side-effect.
  ValidateAccountNames();

  // If the user left some tags unbalanced, issue an error.
  for (const auto& tag : active_tags_) {
    AddError(StrFormat("Unbalanced pushed tag: '%s'", tag), loc);
  }

  // If the user left some metadata unpopped, issue an error.
  for (const auto& [key, value_list] : active_meta_) {
    AddError(StrFormat("Unbalanced metadata key '%s' has leftover metadata", key), loc);
  }

        // # Weave the commas option in the DisplayContext itself, so it propagates
        // # everywhere it is used automatically.
        // self.dcontext.set_commas(self.options['render_commas'])

        // # Build and store the inferred DisplayContext instance.
        // self.options['dcontext'] = self.dcontext

        // # Also record the name of the processed file.
        // self.options['filename'] = filename

        // return sorted(self.entries, key=data.entry_sortkey)

}

// TODO(blais): Process this with code in the plugins loading loop.
//     elif key == 'insert_pythonpath':
//         # Insert the PYTHONPATH to this file when and only if you
//         # encounter this option.
//         sys.path.insert(0, path.dirname(filename))

std::unique_ptr<Ledger> Builder::MakeLedger() {
  auto ledger = std::make_unique<Ledger>();
  ledger->directives = std::move(directives_);
  ledger->errors = std::move(errors_);
  ledger->options = std::move(options_);
  ledger->info = std::move(info_);
  assert(ledger->options);
  assert(ledger->info);
  return ledger;
}

}  // namespace parser
}  // namespace beancount
