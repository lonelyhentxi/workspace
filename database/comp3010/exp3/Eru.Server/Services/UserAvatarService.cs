using Eru.Server.Configurations;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Services.Interfaces;
using Microsoft.Extensions.Options;

namespace Eru.Server.Services
{
    public class UserAvatarService: IUserAvatarService
    {
        private readonly IUrlService _urlService;
        private readonly string _prefix;
        public UserAvatarService(IUrlService urlService, IOptions<UserConfig> options)
        {
            _urlService = urlService;
            _prefix = urlService.JoinUrl(urlService.GetAppUrlPrefix(), options.Value.AvatarBase);
        }
        public string GetDefaultAvatar(User user)
        {
            return _urlService.JoinUrl(_prefix, user.Name);
        }
    }
}
