"""A plugin that asserts that all Commodity directives have a particular
attribute and that it is part of a set of enum values.

The configuration must be a mapping of attribute name to list of valid values,
like this:

  plugin "beancount.plugins.commodity_attr" "{
    'sector': ['Technology', 'Financials', 'Energy'],
    'name': None,
  }"

The plugin issues an error if a Commodity directive is missing the attribute, or
if the attribute value is not in the valid set. If you'd like to just ensure the
attribute is set, set the list of valid values to None, as in the 'name'
attribute in the example above.

"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data

__plugins__ = ('validate_commodity_attr',)

ConfigError = collections.namedtuple('ConfigError', 'source message entry')
CommodityError = collections.namedtuple('CommodityError', 'source message entry')


def validate_commodity_attr(entries, unused_options_map, config_str):
    """Check that all Commodity directives have a valid attribute.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
      config_str: A configuration string.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []

    config_obj = eval(config_str, {}, {})
    if not isinstance(config_obj, dict):
        errors.append(ConfigError(
            data.new_metadata('<commodity_attr>', 0),
            "Invalid configuration for commodity_attr plugin; skipping.", None))
        return entries, errors

    validmap = {attr: frozenset(values) if values is not None else None
                for attr, values in config_obj.items()}
    for entry in entries:
        if not isinstance(entry, data.Commodity):
            continue
        for attr, values in validmap.items():
            value = entry.meta.get(attr, None)
            if value is None:
                errors.append(CommodityError(
                    entry.meta,
                    "Missing attribute '{}' for Commodity directive {}".format(
                        attr, entry.currency), None))
                continue
            if values and value not in values:
                errors.append(CommodityError(
                    entry.meta,
                    "Invalid attribute '{}' for Commodity directive {}; valid options: {}".format(
                        value, entry.currency, ', '.join(values)), None))

    return entries, errors
