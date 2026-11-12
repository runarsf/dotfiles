{ lib', ... }:
lib'.mkServiceUser {
  username = "blahaj";
  # Pinned: /data is owned by it, and its quadlets live in
  # /etc/containers/systemd/users/<uid>.
  uid = 10001;
}
