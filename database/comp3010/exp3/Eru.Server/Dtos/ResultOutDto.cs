using System;
using System.ComponentModel.DataAnnotations;

namespace Eru.Server.Dtos
{
    public class ResultOutDto<TBody>
    {
        [Required]
        public bool success { get; set; }
        public Exception inner { get; set; }
        public string message { get; set; }

        public TBody body { get; set; }
    }

    public class ResultOutDtoBuilder
    {
        public static ResultOutDto<TBody> Success<TBody>(TBody body, string message = "")
        {
            return new ResultOutDto<TBody>
            {
                success = true,
                inner = null,
                message = message,
                body = body,
            };
        }

        public static ResultOutDto<TBody> Fail<TBody>(Exception e, string message)
        {
            return new ResultOutDto<TBody>
            {
                success = false,
                inner = e,
                message = message
            };
        }
    } 
}
