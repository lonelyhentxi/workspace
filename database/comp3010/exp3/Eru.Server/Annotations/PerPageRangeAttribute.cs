using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Threading.Tasks;

namespace Eru.Server.Annotations
{
    public class PerPageRangeAttribute: ValidationAttribute
    {
        private readonly RangeAttribute _rangeAttribute;
        public PerPageRangeAttribute(): base($"PerPage should bigger than {1} and lower than {100}")
        {
            _rangeAttribute = new RangeAttribute(1, 100);
        }

        public override bool IsValid(object value)
        {
            return _rangeAttribute.IsValid(value);
        }
    }
}
