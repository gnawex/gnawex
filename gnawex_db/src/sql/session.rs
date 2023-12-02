pub const SET_CURRENT_USER_ID: &str = "SELECT set_config('auth.user_id', ($1::BIGINT)::TEXT, true)";
pub const LOGIN: &str = "SELECT * FROM api.login($1, $2)";
pub const GET_CURRENT_USER: &str = "SELECT api.current_user()";
pub const USER_ID_FROM_SESSION_TOKEN: &str = "SELECT auth.session_user_id($1)";
pub const SET_ANON_ROLE: &str = "SET LOCAL ROLE anon";
pub const SET_SESSION_TOKEN: &str = "SELECT set_config('request.session_token', $1, true)";
pub const AUTHENTICATE: &str = "SELECT auth.authenticate()";
pub const REFRESH_SESSION: &str = "SELECT api.refresh_session()";
