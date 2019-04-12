using Eru.Server.Data.Models;
using Eru.Server.Services.Interfaces;

namespace Eru.Server.Services
{
    public class UserProfileService : IUserProfileService
    {
        public string GetDefaultMisc(User user)
        {
            return "{}";
        }

        public string GetDefaultSetting(User user)
        {
            return "{}";
        }
    }
}