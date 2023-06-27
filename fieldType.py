
class fieldType (object):
  def __init__ (self, **kwargs):
    self.__dict__ = kwargs
    self.name = 'FIELD_%s%s' % (self.rank, self.suffix);
    self.rank = int (self.rank)
    self.shape = ','.join ([':'] * int (self.rank))
    self.viewRank = self.rank-1
    self.viewShape = ','.join ([':'] * (self.rank-1))

