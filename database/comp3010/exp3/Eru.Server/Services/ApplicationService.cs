using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Eru.Server.Data;
using Eru.Server.Data.Models;
using Eru.Server.Data.Utils;
using Eru.Server.Dtos;
using Eru.Server.Exceptions;
using Microsoft.EntityFrameworkCore;

namespace Eru.Server.Services
{
    public class ApplicationService
    {
        private readonly EruContext _context;
        private readonly ApplicationProfileService _applicationProfileService;

        public ApplicationService(EruContext context,ApplicationProfileService applicationProfileService)
        {
            _context = context;
            _applicationProfileService = applicationProfileService;
        }

        public async Task<List<Application>> Filter(ApplicationFilterInDto filterOptions)
        {
            var filtered = _context.Applications
                .TimeRangeFilter(filterOptions)
                .Where(app =>
                    (string.IsNullOrWhiteSpace(filterOptions.NameMatch) || app.Name.Contains(filterOptions.NameMatch)));
            var ordered = filterOptions.CreateTimeDesc
                ? filtered.OrderByDescending(a => a.CreateTime)
                : filtered.OrderBy(a => a.CreateTime);
            return await ordered.SkipTakePaging(filterOptions).ToListAsync();
        }

        public async Task<Application> Create(ApplicationCreateInDto createOptions)
        {
            if (await _context.Applications.AnyAsync(a => a.Name == createOptions.Name))
            {
                throw new ExistedConflictException();
            }
            using (var transaction = await _context.Database.BeginTransactionAsync())
            {
                var now = DateTime.Now;
                var application = new Application
                {
                    Name = createOptions.Name,
                    Version = createOptions.Version,
                    Avatar = createOptions.Avatar,
                    Url = createOptions.Url,
                    Description = createOptions.Description,
                    CreateTime = now,
                    UpdateTime = now,
                };
                await _context.Applications.AddAsync(application);
                await _context.SaveChangesAsync();
                var applicationProfile = new ApplicationProfile
                {
                    Id = application.Id,
                    Misc = _applicationProfileService.GetDefaultMisc(application),
                    Setting = _applicationProfileService.GetDefaultSetting(application),
                };
                await _context.ApplicationProfiles.AddAsync(applicationProfile);
                await _context.SaveChangesAsync();
                transaction.Commit();
                return application;
            }
        }

        public async Task<Application> Update(Application application)
        {
            if (null == await _context.Applications.FindAsync(application.Id))
            {
                throw new NotExistedException();
            }

            if (await _context.Applications.AnyAsync(a => a.Id != application.Id && a.Name == application.Name))
            {
                throw new ExistedConflictException();
            }

            _context.Entry(application).State = EntityState.Modified;
            _context.Entry(application.Profile).State = EntityState.Unchanged;
            if (application.Profile!=null)
            {
                _context.Entry(application.Profile).State = EntityState.Modified;
            }

            application.UpdateTime = DateTime.Now;
            await _context.SaveChangesAsync();
            return application;
        }

        public async Task<Application> Get(Guid id,bool detailed)
        {
            var application = await _context.Applications
                .FindAsync(id);
            if (application == null)
            {
                throw new NotExistedException();
            }
            if (detailed)
            {
                var applicationProfile = await _context.ApplicationProfiles.FindAsync(id);
                application.Profile = applicationProfile;
            }

            return application;
        }

        public async Task<Application> Remove(Guid id)
        {
            var application = await _context.Applications
                .FindAsync(id);
            if (application == null)
            {
                throw new NotExistedException();
            }

            _context.Applications.Remove(application);
            await _context.SaveChangesAsync();
            return application;
        }
    }
}