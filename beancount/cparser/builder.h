// Builder class used by the parser to build an AST.
// This object contains the state during parisng of a single file.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CPARSER_BUILDER_H_
#define BEANCOUNT_CPARSER_BUILDER_H_

#include "beancount/cparser/scanner.h"
#include "beancount/cparser/location.h"

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "decimal.hh"

namespace beancount {
namespace parser {

// In-memory build state for graph of nodes. This driver accepts actions from
// the parser and simply builds up the parsed state. Since the input is not
// assumed to be ordered, resolving references occurs as a separate phase.
class Builder {
public:
  typedef absl::flat_hash_map<std::string, std::vector<MetaValue*>> MetaMap;

  Builder(const Builder& other) = delete;
  Builder(scanner::Scanner& scanner);
  virtual ~Builder();

  // Return the decimal context in effect during parsing this file.
  inline decimal::Context& context() { return context_; }

  // Return the list of active tags or metadata.
  inline const absl::flat_hash_set<std::string>& active_tags() const {
    return active_tags_;
  }
  inline const MetaMap& active_meta() const {
    return active_meta_;
  }

  // Initialize the global context.
  void Initialize();

  // Add a new option. This gets stored into the options proto and some of the
  // values may influence parsing.
  void AddOption(const std::string& key, std::string&& value,
                 const location& loc);

  // Add an include directive.
  void AddInclude(std::string&& filename);

  // Add a plugin to be run.
  void AddPlugin(std::string&& name, const std::optional<std::string>& config);

  // Intern account and add to the global list of accounts seen.
  const std::string& Account(std::string&& account);

  // Given a filename, if it is relative, return an absolute filename, based on
  // the filename being parsed.
  std::string MakeAbsolutePath(const std::string& filename);

  //---------------------------------------------------------------------------
  // Tags & links

  // Update the given repeated field with the list of active tags.
  // This function mutates the `directive` argument.
  // `tags_links` is optional.
  void SetTagsAndLinks(const TagsLinks* tags_links, Directive* message) const;

  // Push a tag on the context stack.
  void PushTag(const std::string& tag);

  // Pop a tag from the context stack.
  void PopTag(const std::string& tag, const location& loc);

  //---------------------------------------------------------------------------
  // Active metadata

  // Push some metadata key/value pair on the context stack.
  // Steal the reference to `value`.
  void PushMeta(std::string_view key, MetaValue* value);

  // Pop some metadata key pair from the context stack.
  void PopMeta(const std::string& key, const location& loc);

  // Insert the active metadata into the given argument. If there is no metadata
  // to be added, the directive is unmodified.
  void AddActiveMetadata(Meta* meta, Directive* dir) const;

  //---------------------------------------------------------------------------
  // Numbers & precision

  // Convert a decimal number to a proto.
  inline void DecimalProto(const decimal::Decimal& dec, Number* proto) {
    // Note you could configure conversion options here.
    DecimalToProto(dec, true, proto);
  }

  // Check collisions on merging two components of a cost list and log errors
  // appropriately. Mutates the
  absl::Status MergeCost(const inter::CostSpec& new_cost_spec, inter::CostSpec* accumulator);

  // Update statistics on on precision.
  void WitnessDecimal(const decimal::Decimal& dec, const std::string& currency);

  // Create a new directive. Return value ownership is given. If `meta` is
  // non-null, it will be reset and ownership will be taken (and it will be
  // deleted).
  Directive* MakeDirective(const absl::CivilDay& date,
                           Meta** meta,
                           TagsLinks** tags_links) const;

  // Note: We steal ownership.
  void AppendDirective(Directive* directive);

  // Common posting preparation actions.
  void PreparePosting(Posting* posting,
                      const Amount* units_spec,
                      const char flag,
                      const std::string& account,
                      bool is_total,
                      const location& loc);

  // Insert an error in the error log. These will be returned along with the
  // ledger as output of the parsing phase.
  void AddError(std::string_view message, const location& loc);

  // Check that all the accounts are matching the prefix names provided in the
  // options.
  void ValidateAccountNames();

  // Finalize the parser before deletion.
  void Finalize(const location& loc);

  // Build an instance of a new ledger. This is how one stamps and extracts all
  // the results from the parser. The parser isn't reset.
  std::unique_ptr<Ledger> MakeLedger();

private:
  // The accumulated state, same as Ledger.
  std::list<Directive*> directives_;
  std::vector<Error*> errors_;
  std::shared_ptr<options::Options> options_;
  std::shared_ptr<options::ProcessingInfo> info_;

  // Decimal context.
  decimal::Context context_;

  // Scanner (in order to get the last location).
  scanner::Scanner& scanner_;

  // A set of all unique account names seen in the file.
  absl::flat_hash_set<std::string> accounts_;

  // A set of active tags.
  absl::flat_hash_set<std::string> active_tags_;

  // A dict of the current active metadata fields; each of the field values is a
  // stack and the current value is at the top (last value).
  absl::flat_hash_map<std::string, std::vector<MetaValue*>> active_meta_;
};

}  // namespace parser
}  // namespace beancount

#endif // BEANCOUNT_CPARSER_BUILDER_H_
