pub type Role {
  TraderRole
  ModRole
  AdminRole
}

pub type User {
  User(
    name: String,
    email: String,
    is_verified: Bool,
    is_banned: Bool,
    role: Role
  )
}

pub opaque type Trader {
  Trader(name: String, email: String)
}

pub type UserError {
  UnverifiedUser
  BannedUser
  RoleMismatch
}

/// A smart constructor for `Trader`
pub fn mk_trader(user: User) -> Result(Trader, UserError) {
  case user {
    User(name: name, email: email, is_verified: True, is_banned: False, role: TraderRole) ->
      Ok(Trader(name: name, email: email))

    User(name: _, email: _, is_verified: False, is_banned: _, role: _) ->
      Error(UnverifiedUser)

    User(name: _, email: _, is_verified: _, is_banned: True, role: _) ->
      Error(BannedUser)

    _ -> Error(RoleMismatch)
  }
}
