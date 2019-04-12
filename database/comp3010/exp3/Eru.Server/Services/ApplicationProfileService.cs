using Eru.Server.Data.Models;

namespace Eru.Server.Services
{
    public class ApplicationProfileService
    {
        public string GetDefaultMisc(Application application)
        {
            return "{}";
        }

        public string GetDefaultSetting(Application application)
        {
            return "{}";
        }
    }
}
