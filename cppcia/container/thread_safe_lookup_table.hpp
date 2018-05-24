#pragma once

#include <memory>
#include <mutex>

#include <boost/thread/shared_mutex.hpp>
#include <boost/thread/locks.hpp>

#include <hash_map>
#include <utility>
#include <list>
#include <vector>
#include <map>

namespace eevent
{

    using std::hash;
    using std::pair;
    using std::list;
	using std::vector;
	using std::map;
	
	using std::unique_ptr;
	using std::unique_lock;
	
	using boost::shared_mutex;
	using boost::shared_lock;


    template<typename Key,typename Value,typename Hash=hash<Key> >
    class thread_safe_lookup_table
    {
    private:
        class bucket_type
        {
        private:
            using bucket_value =  pair<Key,Value>;
            using bucket_data = list<bucket_value>;
            using  bucket_iterator = typename bucket_data::iterator;

            bucket_data data;
            mutable shared_mutex mutex;

            bucket_iterator find_entry_for(Key const& key) const
            {
                return find_if(data.begin(),data.end(),
                                    [&](bucket_value const& item)
                                    {return item.first==key;});
            }
        public:
            Value value_for(Key const& key,Value const& default_value) const
            {
                shared_lock<shared_mutex> lock(mutex);
                bucket_iterator const found_entry=find_entry_for(key);
                return (found_entry==data.end())?
                       default_value : found_entry->second;
            }

            void add_or_update_mapping(Key const& key,Value const& value)
            {
                unique_lock<shared_mutex> lock(mutex);
                bucket_iterator const found_entry=find_entry_for(key);
                if(found_entry==data.end())
                {
                    data.push_back(bucket_value(key,value));
                }
                else
                {
                    found_entry->second=value;
                }
            }

            void remove_mapping(Key const& key)
            {
                unique_lock<shared_mutex> lock(mutex);
                bucket_iterator const found_entry=find_entry_for(key);
                if(found_entry!=data.end())
                {
                    data.erase(found_entry);
                }
            }
        };

        vector<unique_ptr<bucket_type> > buckets;
        Hash hasher;

        bucket_type& get_bucket(Key const& key) const
        {
            size_t const bucket_index=hasher(key)%buckets.size();
            return *buckets[bucket_index];
        }

    public:
        typedef Key key_type;
        typedef Value mapped_type;
        typedef Hash hash_type;

        thread_safe_lookup_table(
                unsigned const num_buckets =19, Hash const& hasher_=Hash()):
                buckets(num_buckets),hasher(hasher_)
        {
            for(unsigned i=0;i<num_buckets;++i)
            {
                buckets[i].reset(new bucket_type);
            }
        }

        thread_safe_lookup_table(thread_safe_lookup_table const& other)=delete;
        thread_safe_lookup_table& operator=(
                thread_safe_lookup_table const& other)=delete;

        Value value_for(Key const& key,
                        Value const& default_value=Value()) const
        {
            return get_bucket(key).value_for(key,default_value);
        }

        void add_or_update_mapping(Key const& key,Value const& value)
        {
            get_bucket(key).add_or_update_mapping(key,value);
        }

        void remove_mapping(Key const& key)
        {
            get_bucket(key).remove_mapping(key);
        }

		map<Key,Value> get_map() const
        {
			vector<unique_lock<shared_mutex>> locks;
			for (unsigned i = 0; i<buckets.size(); ++i)
			{
				locks.push_back(unique_lock<shared_mutex>(buckets[i].mutex));
			}
			map<Key, Value> res;
			for (unsigned i = 0; i<buckets.size(); ++i)
			{
				for (auto it = buckets[i].data.begin();
					it != buckets[i].data.end();
					++it)
				{
					res.insert(*it);
				}
			}
			return res;
        }
    };
}
