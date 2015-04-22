%compile
erl -make 

%run
erl -pa ebin/
make:all([load]).
application:start(s5_server).
application:which_applications().
application:stop(s5_client_app).
