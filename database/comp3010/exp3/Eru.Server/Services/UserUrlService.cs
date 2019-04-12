using Eru.Server.Configurations;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Services.Interfaces;
using Microsoft.Extensions.Options;

namespace Eru.Server.Services
{
    public class UserUrlService: IUserUrlService
    {
        private readonly string _prefix;
        private readonly IUrlService _urlService;

        public UserUrlService(IUrlService urlService, IOptions<UserConfig> userOptions)
        {
            _urlService = urlService;
            _prefix = urlService.JoinUrl(urlService.GetAppUrlPrefix(), userOptions.Value.UrlBase);
        }
        public string GetDefaultUrl(User user)
        {
            return _urlService.JoinUrl(_prefix, user.Name);
        }
    }
}
