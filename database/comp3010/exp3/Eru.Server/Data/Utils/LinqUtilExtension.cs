using System.Linq;
using Eru.Server.Data.Interfaces;
using Eru.Server.Dtos.Interfaces;

namespace Eru.Server.Data.Utils
{
    public static class LinqUtilExtension
    {
        public static IQueryable<TSource> TimeRangeFilter<TSource>(
            this IQueryable<TSource> sources,
            ITimeRangeFilterInDto filterOptions) where TSource : ITimeEntity
        {
            var results = sources;
            if (!(filterOptions.CreateTimeFrom is null))
            {
                results = results.Where(s => s.CreateTime >= filterOptions.CreateTimeFrom);
            }

            if (!(filterOptions.CreateTimeTo is null))
            {
                results = results.Where(s => s.CreateTime <= filterOptions.CreateTimeTo);
            }

            if (!(filterOptions.UpdateTimeFrom is null))
            {
                results = results.Where(s => s.UpdateTime >= filterOptions.UpdateTimeFrom);
            }

            if (!(filterOptions.UpdateTimeTo is null))
            {
                results = results.Where(s => s.UpdateTime <= filterOptions.UpdateTimeTo);
            }

            return results;
        }

        public static IQueryable<TSource> SkipTakePaging<TSource>(
            this IQueryable<TSource> sources,
            IPaging filterOptions)
        {
            return sources
                .Skip(filterOptions.PerPage * (filterOptions.Page - 1))
                .Take(filterOptions.PerPage);
        }
    }
}