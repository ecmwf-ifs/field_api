#Copyright 2023 Meteo-France, ECMWF 
#
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.


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


kinds = ['JPRM', 'JPRB', 'JPRD', 'JPIM', 'JPLM']


def getFieldTypeList (ranks=[2,3,4,5], kinds=kinds):
  return [fieldType (kind=kind, rank=rank) for (kind) in kinds for rank in ranks]

def useParkind1 (kinds=kinds):
  return 'USE PARKIND1, ONLY : ' + ', '.join (kinds)
