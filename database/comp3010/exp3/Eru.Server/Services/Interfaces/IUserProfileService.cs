using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data.Models;

namespace Eru.Server.Services.Interfaces
{
    public interface IUserProfileService
    {
        string GetDefaultMisc(User user);

        string GetDefaultSetting(User user);
    }
}
