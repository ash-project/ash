import Ash.Type.Comparable

defcomparable left :: Decimal, right :: Integer do
  Decimal.compare(left, Ash.Type.Decimal.new(right))
end

defcomparable left :: Decimal, right :: Decimal do
  Decimal.compare(left, right)
end

defcomparable left :: Decimal, right :: Float do
  Decimal.compare(Ash.Type.Decimal.new(left), right)
end

defcomparable left :: Decimal, right :: BitString do
  Decimal.compare(left, Ash.Type.Decimal.new(right))
end
