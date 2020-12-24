#include "beancount/cparser/builder.h"
#include "beancount/ccore/std_utils.h"

#include <experimental/filesystem>

#include "google/protobuf/descriptor.pb.h"
#include "google/protobuf/text_format.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_cat.h"

namespace beancount {
namespace parser {
namespace filesystem = std::experimental::filesystem;
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

// TODO(blais): Review this, not needed.
void Builder::Initialize() {
  //// kAccountRE(StrFormat("(?:%s)(?:%s%s)+",

  // Set Decimal context before processing, update the desired precision for
  // arithmetic operations.
  //
  // Note: Review this, perhaps allow the user to override it (would require
  // post-poning the computation of arithmetic expressions).
  context_ = decimal::context;
  context_.prec(28);
}

void Builder::AddOption(const string& key, string&& value) {
  // Check the options field and get relevant descriptors.
  const auto* descriptor = options_->GetDescriptor();
  const auto* field = descriptor->FindFieldByName(key);
  if (!field) {
    LogError(StrFormat("Invalid option: '%s'", key));
    return;
  }

  // Issue a warning if the option is deprecated.
  if (field->options().deprecated()) {
    LogError(StrFormat("Option '%s' is deprecated; ignored.", key));
    return;
  }

  // Set the field on the options proto.
  string value_str = std::move(value);
  if (field->type() == FieldDescriptor::TYPE_STRING) {
    value_str = absl::StrCat("\"", absl::CEscape(value_str), "\"");
  }
  if (!TextFormat::ParseFieldValueFromString(value_str, field, options_.get())) {
    LogError(StrFormat("Could not parse and set option '%s' with value '%s'; ignored.",
                       key, value));
    return;
  }

  // TODO(blais): Handle "inferred_tolerance_default" after review, it's for a
  // map. They require special handling. This is a map.
  //
  //      [Opt("inferred_tolerance_default", {}, "CHF:0.01",
  //           converter=options_validate_tolerance_map)]),

  // TODO(blais): Do this.
  // This just requires converted to a Decimal object.
  //       Opt("inferred_tolerance_multiplier", D("0.5"), "1.1",
  //           converter=D)]),
  //   ParserError(meta, "Error for option '{}': {}".format(key, exc),
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

const string& Builder::Account(string&& account) {
  auto iter = accounts_.insert(account);
  return *iter.first;
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

void Builder::PopTag(const string& tag) {
  auto iter = active_tags_.find(tag);
  if (iter != active_tags_.end()) {
    active_tags_.erase(iter);
  } else {
    LogError(StrFormat("Attempting to pop absent tag: '%s'", tag));
  }
}

void Builder::PushMeta(std::string_view key, MetaValue* value) {
  auto& value_list = active_meta_[key];
  value_list.push_back(value);
}

void Builder::PopMeta(const string& key) {
  auto iter = active_meta_.find(key);
  if (iter == active_meta_.end()) {
    LogError(StrFormat("Attempting to pop absent metadata key: '%s'", key));
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

  // TODO(blais): Check for the presence of duplicates in the loop above and
  // log and error if found.
  //
  //  value = explicit_meta.setdefault(posting_or_kv.key,
  //                                   posting_or_kv.value)
  //  if value is not posting_or_kv.value:
  //      self.errors.append(ParserError(
  //          meta, "Duplicate metadata field on entry: {}".format(
  //              posting_or_kv), None))
  //
}

void Builder::MergeCost(const inter::CostSpec& new_cost_spec, inter::CostSpec* accumulator) {
  // TODO(blais): Check for collisions here.
  accumulator->MergeFrom(new_cost_spec);
}


void Builder::UpdatePrecisionStats(const decimal::Decimal& dec, const string& currency) {
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
                             const char flag,
                             const string& account,
                             bool is_total,
                             const location& loc) {
  assert(posting != nullptr);

  // Store flag and account name.
  if (flag != '\0') {
    posting->set_flag(&flag, 1);
  }
  posting->set_account(account);

  // Check conditions renamed to price annotations.
  if (posting->has_price()) {
    const auto& price = posting->price();

    // If the price is specified for the entire amount, compute the effective
    // price here and forget about that detail of the input syntax.
    if (is_total) {
      if (!posting->has_units() || !posting->units().has_number()){
        // units.number is MISSING.
        // Note: we could potentially do a better job and attempt to f
        // this up after interpolation, but this syntax is pretty rare
        // anyway.
        LogError(StrFormat("Total price on a posting without units: %s.",
                           price.DebugString()));
        posting->clear_price();
      } else if (price.has_number()) {
        decimal::Decimal dunits = ProtoToDecimal(posting->units().number());
        decimal::Decimal dprice;
        if (dunits.iszero()) {
          dprice = dunits;
        } else {
          dprice = ProtoToDecimal(price.number()).div(dunits.abs(), context());
        }
        DecimalProto(dprice, posting->mutable_price()->mutable_number());
      }
    }

    // Note: Allow zero prices because we need them for round-trips for
    // conversion entries.
    //
    // if price is not None and price.number == ZERO:
    //     self.errors.append(
    //         ParserError(meta, "Price is zero: {}".format(price), None))

    // If both cost and price are specified, the currencies must match, or
    // that is an error.
    if (posting->has_cost_spec() &&
        posting->cost_spec().has_currency() &&
        price.has_currency() &&
        posting->cost_spec().currency() != price.currency()) {
      LogError(StrFormat("Cost and price currencies must match: %s != %s",
                         posting->cost_spec().currency(), price.currency()));
    }
  }
}


void Builder::LogError(const string& message) {
  auto* error = new Error();
  errors_.push_back(error);
  error->set_message(Capitalize(message));

  // TODO(blais): add location
}
// TODO(blais): Can we turn this error logging into a stream instead?

void Builder::ValidateAccountNames() {
  // TODO(blais):

  // Validate the account root names are valid in the options.

  //     # Refresh the list of valid account regexps as we go along.
  //     if key.startswith('name_'):
  //         # Update the set of valid account types.
  //         self.account_regexp = valid_account_regexp(self.options)

  // if not self.account_regexp.match(account):
  //     meta = new_metadata(filename, lineno)
  //     self.errors.append(
  //         ParserError(meta, "Invalid account name: {}".format(account), None))
}

void Builder::Finalize() {
  void ValidateAccountNames();

  // If the user left some tags unbalanced, issue an error.
  for (const auto& tag : active_tags_) {
    LogError(StrFormat("Unbalanced pushed tag: '%s'", tag));
  }

  // If the user left some metadata unpopped, issue an error.
  for (const auto& [key, value_list] : active_meta_) {
    LogError(StrFormat(
                 "Unbalanced metadata key '%s' has leftover metadata", key));
  }
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
