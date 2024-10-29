def get_token_from_keyring():
  import subprocess
  cmd = "lssecret -s | grep -i -A1 'grip' | tail -n 1 | sed 's/^secret:[[:space:]]//I'"
  process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  output, error = process.communicate()

  if process.returncode != 0:
    raise Exception(f"error retrieving access token from keyring: {error.decode().strip()}")

  return output.decode().strip()

PASSWORD = get_token_from_keyring()
USERNAME = 'neoveil'
