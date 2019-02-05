using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace Codewars.Fourkyu
{
    public class AddingBigNumbers
    {
        internal readonly int _sign;
        internal readonly uint[] _bits;

        public AddingBigNumbers(string s)
        {
            int smallIntResult;
            if (int.TryParse(s, out smallIntResult))
            {
                if (smallIntResult > int.MinValue)
                {
                    _sign = smallIntResult;
                    _bits = (uint[]) null;
                }
            }
        }



        public static string Add(string lhs, string rhs)
        {
            var lArray = lhs.Select(ch => uint.Parse(ch.ToString())).ToArray();
            var rArray = rhs.Select(ch => uint.Parse(ch.ToString())).ToArray();
            var minLength = Math.Min(lArray.Length, rArray.Length);
            var lIndex = lArray.Length;
            var rIndex = rArray.Length;
            var target = rhs.
            for (var i = minLength - 1; i >= 0; i--)
            {
                BigInteger a = 1;
                BigInteger b = 2;
                var c = a + b;
            }
        }
    }
}
