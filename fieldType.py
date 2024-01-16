# (C) Copyright 2022- ECMWF.
# (C) Copyright 2022- Meteo-France.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

class fieldType (object):
  def __init__ (self, **kwargs):
    self.__dict__ = kwargs
    self.suffix = self.kind[2:]

    tt = self.suffix[0]
    ss = self.suffix[1]

    th = {'R': 'REAL', 'I': 'INTEGER', 'L': 'LOGICAL'}
    td = {'R':  '0.0', 'I':       '0', 'L': '.FALSE.'}

    self.type = th[tt] + '(KIND=' + self.kind + ')'
    self.default = td[tt] + '_' + self.kind
    self.alias = self.suffix == 'RB'

    self.name = 'FIELD_%s%s' % (self.rank, self.suffix);
    self.rank = int (self.rank)
    self.shape = ','.join ([':'] * int (self.rank))
    self.viewRank = self.rank-1
    self.viewShape = ','.join ([':'] * (self.rank-1))
    self.lbptr = ', '.join (list (map (lambda i: "LBOUNDS(" + str (i+1) + "):", range (0, self.rank))))
    self.hasView = self.rank > 1
    self.ganged = self.rank > 2


kinds = ['JPRM', 'JPRB', 'JPRD', 'JPIM', 'JPLM']

def eqv (a, b):
  if (a) and (b):
    return True
  if (not a) and (not b):
    return True
  return False

def getFieldTypeList (ranks=[1,2,3,4,5], kinds=kinds, hasView=None, alias=None, ganged=None):
  
  l = [fieldType (kind=kind, rank=rank) for (kind) in kinds for rank in ranks]

  if not (hasView is None):
    l = list (filter (lambda ft: eqv (ft.hasView, hasView), l))
  
  if not (alias is None):
    l = list (filter (lambda ft: eqv (ft.alias, alias), l))
  
  if not (ganged is None):
    l = list (filter (lambda ft: eqv (ft.ganged, ganged), l))
  
  return l

def useParkind1 (kinds=kinds):
  return 'USE PARKIND1, ONLY : ' + ', '.join (kinds)
