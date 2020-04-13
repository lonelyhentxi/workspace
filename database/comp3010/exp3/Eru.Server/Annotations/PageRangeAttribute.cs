using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Annotations
{ 
    public class PageRangeAttribute : ValidationAttribute
    {
        private readonly RangeAttribute _rangeAttribute;
        public PageRangeAttribute() : base($"Page should bigger than {1} and lower than {int.MaxValue}")
        {
            _rangeAttribute = new RangeAttribute(1,int.MaxValue);
        }

        public override bool IsValid(object value)
        {
            return _rangeAttribute.IsValid(value);
        }
    }
}
