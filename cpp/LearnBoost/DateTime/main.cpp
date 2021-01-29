#include <iostream>
#include <ctime>
#include <limits>
#include <vector>
#include <boost/timer.hpp>
#include <boost/progress.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#define let const auto
using namespace std;
using namespace boost;

boost::gregorian::days operator"" _D(unsigned long long n)
{
	return boost::gregorian::days(n);
}

boost::gregorian::weeks operator"" _W(unsigned long long n)
{
	return boost::gregorian::weeks(n);
}

boost::gregorian::date operator"" _YMD(const char* s, std::size_t)
{
	return gregorian::from_string(s);
}

int main()
{
	{
		// timer v1
		{
			// simple example
			let t = timer{};
			cout << "max timespan:" << t.elapsed_max() / 4600 << "h" << endl;
			cout << "min timespan:" << t.elapsed_min() << "s" << endl;
			cout << "now time elapsed:" << t.elapsed() << "s" << endl;
		}
		{
			// timer impl
			class my_timer
			{
			public:
				my_timer ()
				{
					_start_timer = std::clock();
				}

			void restart()
				{
				_start_timer = std::clock();
				}
			constexpr double elapsed_min() const
				{
				return static_cast<double>(1) / static_cast<double>(CLOCKS_PER_SEC);
				}
			double elapsed_max() const
				{
				return (static_cast<double>(std::numeric_limits<std::clock_t>::max()
					- static_cast<double>(_start_timer)) / static_cast<double>(CLOCKS_PER_SEC));
				}
			double elapsed() const
				{
				return (static_cast<double>(std::clock()) - static_cast<double>(_start_timer)) / (static_cast<double>(CLOCKS_PER_SEC));
				}
			private:
				std::clock_t _start_timer;
			};

			let t = my_timer{};
			cout << "max timespan:" << t.elapsed_max() / 4600 << "h" << endl;
			cout << "min timespan:" << t.elapsed_min() << "s" << endl;
			cout << "now time elapsed:" << t.elapsed() << "s" << endl;
		}
		{
			// progress timer
			progress_timer  t;
			auto vec = std::vector<int64_t>(200000, 0);
			for(size_t i = 0; i <= 100; i++)
			{
				for(auto & v: vec)
				{
					v += 1;
				}
			} 
		}
		{
			// my progress timer
			class my_progress_timer: public timer, public boost::noncopyable
			{
			public:
				explicit  my_progress_timer(std::ostream& os = std::cout) : timer(), noncopyable(), m_os(os) {}

				~my_progress_timer()
				{
					try
					{
						std::istream::fmtflags old_flags = m_os.setf(std::istream::fixed, std::istream::floatfield);

						std::streamsize old_prec = m_os.precision(2);
						m_os << elapsed() << " s\n" << std::endl;
						m_os.flags(old_flags);
						m_os.precision(old_prec);
					}
					catch (...) {}
				}
			private:
				std::ostream& m_os;
			};
		}
	}
	{
		// date_time
		using namespace boost::gregorian;

		{
			// gregorian from 1400-01-01 to 9999-12-31
			// here are specials
			cout << "positive infinite timestamp:" << date(pos_infin) << endl;
			cout << "negative infinite timestamp:" << date(neg_infin) << endl;
			cout << "invalid timestamp:" << date(not_a_date_time) << endl;
			cout << "max date time:" << date(max_date_time) << endl;
			cout << "min date time:" << date(min_date_time) << endl;
		}
		{
			// usage
			{
				date d1;
				date d2(2010, 1, 1);
				date d3(2000, Jan, 1);
				date d4(d2);
				
				assert(d1 == date(not_a_date_time));
				assert(d2 == d4);
				assert(d3 < d4);
			}

			{
				date d1 = from_string("1999-12-31");
				date d2 = from_string("2015/1/1");
				date d3 = from_undelimited_string("20011118");

				cout << day_clock::local_day() << ", " << day_clock::universal_day() << endl;
			}

			{
				// invalid type
				try
				{
					date d1(1399, 12, 1);
				} catch (...) {}
				try
				{
					date d2(10000, 1, 1);
				} catch (...) {}
				try
				{
					date d3(2017, 2, 29);
				} catch (...) {}
			}
		}
		{
			date d(2017, 6, 1);
			assert(d.year() == 2017);
			assert(d.month() == 6);
			assert(d.day() == 1);

			date::ymd_type ymd = d.year_month_day();
			assert(ymd.year == 2017);
			assert(ymd.month == 6);
			assert(ymd.day == 1);

			cout << d.day_of_year() << endl;
			cout << d.day_of_week() << endl;
			assert(d.end_of_month() == date(2017, 6, 30));

			cout << date(2015, 1, 10).week_number() << endl;
			cout << date(2016, 1, 10).week_number() << endl;

			assert(date(pos_infin).is_infinity() && date(pos_infin).is_pos_infinity());
		}
		{
			// date output
			date d(2017, 1, 23);

			cout << to_simple_string(d) << endl;
			cout << to_iso_string(d) << endl;
			cout << to_iso_extended_string(d) << endl;
		}
		{
			date d(2017, 5, 20);
			tm t = to_tm(d);
			assert(t.tm_hour == 0 && t.tm_min == 0);
			assert(t.tm_year == 117 && t.tm_mday == 20);

			date d2 = date_from_tm(t);
			assert(d == d2);
		}
		{
			// date duration
			days dd1(10), dd2(-100), dd3(255);
			assert(dd1 > dd2 && dd1 < dd3);

			date d1(2000, 1, 1), d2(2017, 11, 18);
			cout << d2 - d1 << endl;
		}
		{
			// date period
			date_period dp1(date(2017, 1, 1), days(20));
			date_period dp2(date(2017, 1, 1), days(-20)); // invalid
			assert(!dp1.is_null());
			assert(dp1.begin().day() == 1);
			assert(dp1.last().day() == 20);
			assert(dp1.end().day() == 21);
			assert(dp1.length().days() == 20);

			{
				date_period dp(date(2017, 1, 1), days(20));
				dp.shift(days(3));
				assert(dp.begin().day() == 4);
				assert(dp.length().days() == 20);

				dp.expand(days(3));
				assert(dp.begin().day() == 1);
				assert(dp.length().days() == 26);
			}

			{
				date_period dp(date(2010, 1, 1), days(20));
				assert(dp.is_after(date(2009, 12, 1)));
				assert(dp.contains(date(2010, 1, 10)));

				date_period dp2(date(2010, 1, 5), days(10));
				assert(dp.contains(dp2));

				assert(dp.intersects(dp2));
				assert(dp.intersection(dp2) == dp2);

				date_period dp3(date(2010, 1, 21), days(5));
				assert(dp.is_adjacent(dp3));
				assert(!dp.intersects(dp3));
			}
		}
		{
			// date iterator
			date d(2007, 9, 28);
			day_iterator d_iter(d);

			assert(d_iter == d);
			++d_iter;
			assert(d_iter == date(2007, 9, 29));

			year_iterator y_iter(*d_iter, 10);
			assert(y_iter == d + days(1));
			++y_iter;
			assert(y_iter->year() == 2017);
		}
		{
			typedef gregorian_calendar gre_cal;
			cout << "Y2017 is " << (gre_cal::is_leap_year(2017) ? "" : "not") << " a leap year." << endl;
			assert(gre_cal::end_of_month_day(2017, 2) == 28);
		}
		{
			// combined usage
			{
				// #1
				date d(2017, 1, 23);
				date d_start(d.year(), d.month(), 1);
				date d_end = d.end_of_month();
				for (day_iterator d_iter(d_start); d_iter != d_end; ++d_iter)
				{
					cout << *d_iter << " " << d_iter->day_of_week() << endl;
				}
			}
			{
				// #2
				
			}
		}
	}
	{
		// posix_time
		using namespace boost::posix_time;
		{
			// manufacture time duration
			time_duration td(1, 10, 30, 1000);
			time_duration td1(1, 60, 60, 1000 * 1000 * 6 + 1000);

			hours h(1);
			minutes m(10);
			seconds s(30);
			millisec ms(1);

			time_duration td2 = h + m + s + ms;
			time_duration td3 = hours(2) + seconds(10);

			time_duration td4 = duration_from_string("1:10:30:001");
			assert(td.hours() == 1 && td.minutes() == 10 && td.seconds() == 30);
		}
		{
// 			// time precision
// #define BOOST_DATE_TIME_POSIX_TIME_STD_CONFIG
// #include <boost/date_time/posix_time/posix_time.hpp>
// 			time_duration td(1, 10, 30, 1000);
// 			cout << td;
// 			assert(td.total_microseconds() == td.total_seconds() * 1000);
// 			assert(td.fractional_seconds() == 1000);
// 			assert(time_duration::unit() * 1000 * 1000 * 1000 == seconds(1));
//
// 			assert(td.resolution() == date_time::nano);
// 			assert(td.num_fractional_digits() == 9);
		}
		{
			time_duration::tick_type my_millisec = time_duration::ticks_per_second() / 1000;
			time_duration td(1, 10, 30, 10 * my_millisec);
			cout << td.seconds() << endl;
		}
		{
			// time point
			using namespace boost::gregorian;
			{
				ptime p(date(2017, 7, 7), hours(1));
				ptime p1 = time_from_string("2017-7-7 01:00:00");
				ptime p2 = from_iso_string("20170707T010000");
			}
			{
				ptime p1 = second_clock::local_time();
				ptime p2 = microsec_clock::universal_time();
				cout << p1 << endl << p2;
			}
			{
				ptime p1(not_a_date_time);
				assert(p1.is_not_a_date_time());
				ptime p2(pos_infin);
				assert(p2.is_special() && p2.is_pos_infinity());
			}
			{
				ptime p(date(2017, 2, 14), hours(20));

				cout << to_simple_string(p);
				cout << to_iso_string(p);
				cout << to_iso_extended_string(p);
			}
			{
				// convert to c structure
				{
					ptime p(date(2017, 5, 20), hours(14));
					tm t = to_tm(p);
					assert(t.tm_year == 117 && t.tm_hour == 14);
				}
				{
					ptime p = from_time_t(std::time(0));
					assert(p.date() == day_clock::local_day());
					cout << to_time_t(p) << endl;
				}
			}
			{
				// time iterator
				ptime p(date(2017, 5, 31), hours(10));

				for(time_iterator t_iter(p, minutes(10)); t_iter < p + hours(1); ++ t_iter)
				{
					cout << *t_iter << endl;
				}
			}
		}
		{
			// advanced
			// DATE_TIME_NO_DEFAULT_CONSTRUCTOR => default constructor
			// BOOST_DATE_TIME_OPTIONAL_GREGORIAN_TYPES => use optional types
			// BOOST_DATE_TIME_POSIX_TIME_STD => precision
			{
				auto d = 100_D;
				auto w = 1_W;
				assert(d.days() == 100);
				assert(w.days() == (7_D).days());
				auto today = "2014/11/3"_YMD;
				cout << today << endl;
			}
		}
		{
			// time format
			using namespace boost::posix_time;
			using namespace boost::gregorian;

			date d(2014, 11, 3);
			date_facet* dfacet = new date_facet("%Y 年%m 月%d 日");
			cout.imbue(locale(cout.getloc(), dfacet));
			cout << d << endl;

			time_facet* tfacet = new time_facet("%Y 年%m 月%d 日%H 点%M 分%S%F 秒");
			cout.imbue(locale(cout.getloc(), tfacet));
			cout << ptime(d, hours(21) + minutes(50) + millisec(100)) << endl;
		}
		{
			// local time
			using namespace boost::local_time;
			using namespace boost::gregorian;
			using namespace boost::posix_time;

			{
				tz_database tz_db;
				tz_db.load_from_file("./data_time_zonespec.csv");
				time_zone_ptr shz = tz_db.time_zone_from_region("Asia/Shanghai");
				time_zone_ptr sfz = tz_db.time_zone_from_region("America/Los_Angles");
				cout << shz->has_dst() << endl;
				cout << shz->std_zone_name() << endl;
				local_date_time dt_bj(date(2014, 3, 6), hours(16), shz, false);
				cout << dt_bj << endl;
				time_duration flight_time = hours(12);
				dt_bj += flight_time;
				cout << dt_bj << endl;
				local_date_time dt_sf = dt_bj.local_time_in(sfz);
				cout << dt_sf << endl;
			}
		}
	}
	return 0;
}