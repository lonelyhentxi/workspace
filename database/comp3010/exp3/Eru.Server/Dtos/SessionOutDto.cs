using Eru.Server.Data.Models;

namespace Eru.Server.Dtos
{
    public class SessionOutDto
    {
        public User User { get; set; }
        public dynamic Token { get; set; }
    }
}
