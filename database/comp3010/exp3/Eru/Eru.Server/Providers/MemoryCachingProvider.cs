using Microsoft.Extensions.Caching.Memory;
using Eru.Server.Interfaces;

namespace Eru.Server.Providers
{
    public class MemoryCachingProvider: ICachingProvider
    {
        private readonly IMemoryCache _cache;

        public MemoryCachingProvider(IMemoryCache _cache)
        {

        }
    }
}
