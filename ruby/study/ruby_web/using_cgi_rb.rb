require 'cgi'
require 'cgi/session'

def try_escape_plain
  puts CGI.escape("Nicholas Payton/Trumpet & Flugel Horn")
end

def try_escape_html
  puts CGI.escapeHTML("a <100 && b> 200")
end

def try_escape_elem
  puts CGI.escape_element('<hr><a href="/mp3">Click Here</a><br>', 'A')
end

# There have unescape html
COOKIE_NAME = 'chocolate chip'

def try_cookie
  cgi = CGI.new
  values = cgi.cookies[COOKIE_NAME]
  if values.empty?
    msg = "It looks as if you haven't visited recently"
  else
    msg = "You last visited #{values[0]}"
  end
  cookie = CGI::Cookie.new(COOKIE_NAME, Time.now.to_s)
  cookie.expires = Time.now + 30 * 24 * 3600 # 30 days
  cgi.out("cookie" => cookie) {msg}
end

def try_sessions
  cgi = CGI.new('html3')
  sess = CGI::Session.new(cgi, 'session_key' => 'rubyweb', 'prefix' => 'web-session.')
  if sess['lastaccess']
    msg = "You were last here #{sess['lastaccess']}"
  else
    msg = "Looks like you haven't been here for a while"
  end
  count = (sess['accesscount'] || 0).to_i
  count += 1
  msg << "<p>Number of visits: #{count}"
  sess['accesscount'] = count
  sess['lastaccess'] = Time.now.to_s
  sess.close
  cgi.out {
    cgi.html {
      cgi.body {
        msg
      }
    }
  }
end