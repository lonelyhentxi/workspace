using Eru.Server.Data.Models;

namespace Eru.Server.Services
{
    public class ApplicationProfileService
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
