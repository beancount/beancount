"""
Institution mixin
"""
__copyright__ = "Copyright (C) 2018  Michael Droogleever"
__license__ = "GNU GPLv2"


class InstitutionMixin:
    """
    Institution mixin
    """
    def __init__(self, institution=None, **kwargs):
        """Provide institution to modify file_name"""
        super().__init__(**kwargs)
        self.institution = institution
    def file_name(self, file):
        name = super().file_name(file) #pylint: disable=E1101
        if self.institution:
            return "{}.{}".format(self.institution, name)
        return name
