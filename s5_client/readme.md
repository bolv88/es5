erl -pa ebin/
make:all([load]).
application:start(s5_client).
application:which_applications().
application:stop(s5_client_app).
