#include "beancount/cparser/builder.h"
#include "beancount/ccore/std_utils.h"

#include <filesystem>

#include "google/protobuf/descriptor.pb.h"
#include "google/protobuf/text_format.h"
#include "absl/strings/str_format.h"
#include "absl/strings/str_cat.h"
#include "absl/status/status.h"

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

void Builder::AddOption(const string& key, string&& value, const location& loc) {
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

  // Set the field on the options proto.
  string value_str = std::move(value);
  if (field->type() == FieldDescriptor::TYPE_STRING) {
    value_str = absl::StrCat("\"", absl::CEscape(value_str), "\"");
  }
  if (!TextFormat::ParseFieldValueFromString(value_str, field, options_.get())) {
    AddError(StrFormat("Could not parse and set option '%s' with value '%s'; ignored.",
                       key, value_str), loc);
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


    // def option(self, filename, lineno, key, value):
    //     """Process an option directive.

    //     Args:
    //       filename: current filename.
    //       lineno: current line number.
    //       key: option's key (str)
    //       value: option's value
    //     """
    //     if key not in self.options:
    //         meta = new_metadata(filename, lineno)
    //         self.errors.append(
    //             ParserError(meta, "Invalid option: '{}'".format(key), None))

    //     elif key in options.READ_ONLY_OPTIONS:
    //         meta = new_metadata(filename, lineno)
    //         self.errors.append(
    //             ParserError(meta, "Option '{}' may not be set".format(key), None))

    //     else:
    //         option_descriptor = options.OPTIONS[key]

    //         # Issue a warning if the option is deprecated.
    //         if option_descriptor.deprecated:
    //             assert isinstance(option_descriptor.deprecated, str), "Internal error."
    //             meta = new_metadata(filename, lineno)
    //             self.errors.append(
    //                 DeprecatedError(meta, option_descriptor.deprecated, None))

    //         # Rename the option if it has an alias.
    //         if option_descriptor.alias:
    //             key = option_descriptor.alias
    //             option_descriptor = options.OPTIONS[key]

    //         # Convert the value, if necessary.
    //         if option_descriptor.converter:
    //             try:
    //                 value = option_descriptor.converter(value)
    //             except ValueError as exc:
    //                 meta = new_metadata(filename, lineno)
    //                 self.errors.append(
    //                     ParserError(meta,
    //                                 "Error for option '{}': {}".format(key, exc),
    //                                 None))
    //                 return

    //         option = self.options[key]
    //         if isinstance(option, list):
    //             # Append to a list of values.
    //             option.append(value)

    //         elif isinstance(option, dict):
    //             # Set to a dict of values.
    //             if not (isinstance(value, tuple) and len(value) == 2):
    //                 self.errors.append(
    //                     ParserError(
    //                         meta, "Error for option '{}': {}".format(key, value), None))
    //                 return
    //             dict_key, dict_value = value
    //             option[dict_key] = dict_value

    //         elif isinstance(option, bool):
    //             # Convert to a boolean.
    //             if not isinstance(value, bool):
    //                 value = (value.lower() in {'true', 'on'}) or (value == '1')
    //             self.options[key] = value

    //         else:
    //             # Set the value.
    //             self.options[key] = value

    //         # Refresh the list of valid account regexps as we go along.
    //         if key.startswith('name_'):
    //             # Update the set of valid account types.
    //             self.account_regexp = valid_account_regexp(self.options)
    //         elif key == 'insert_pythonpath':
    //             # Insert the PYTHONPATH to this file when and only if you
    //             # encounter this option.
    //             sys.path.insert(0, path.dirname(filename))
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
        AddError(StrFormat("Total price on a posting without units: %s.",
                           price.DebugString()), loc);
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
      AddError(StrFormat("Cost and price currencies must match: %s != %s",
                         posting->cost_spec().currency(), price.currency()), loc);
    }
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

    // names = map(options.__getitem__, ('name_assets',
    //                                   'name_liabilities',
    //                                   'name_equity',
    //                                   'name_income',
    //                                   'name_expenses'))

    // # Replace the first term of the account regular expression with the specific
    // # names allowed under the options configuration. This code is kept in sync
    // # with {5672c7270e1e}.
    // return re.compile("(?:{})(?:{}{})+".format('|'.join(names),
    //                                            account.sep,
    //                                            account.ACC_COMP_NAME_RE))

}

void Builder::Finalize(const location& loc) {
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
