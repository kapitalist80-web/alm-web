#!/usr/bin/env bash
# -------------------------------------------------------------------
# setup-ec2.sh – Bootstrap an EC2 instance (Amazon Linux 2023 / Ubuntu)
# for running the ALM-Web app with Docker and HTTPS.
#
# Usage:
#   chmod +x setup-ec2.sh
#   sudo ./setup-ec2.sh
#
# Prerequisites:
#   - A domain name pointing (A record) to this EC2 instance's public IP
#   - Security Group allows inbound ports 80 and 443
# -------------------------------------------------------------------
set -euo pipefail

# ── 1. Detect OS ─────────────────────────────────────────────────────
if [ -f /etc/os-release ]; then
    . /etc/os-release
    OS_ID="$ID"
else
    echo "Cannot detect OS. Exiting."
    exit 1
fi

echo "==> Detected OS: $OS_ID"

# ── 2. Install Docker ───────────────────────────────────────────────
install_docker_amazon_linux() {
    dnf update -y
    dnf install -y docker git
    systemctl enable --now docker
    usermod -aG docker ec2-user
}

install_docker_ubuntu() {
    apt-get update -y
    apt-get install -y ca-certificates curl gnupg
    install -m 0755 -d /etc/apt/keyrings
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | \
        gpg --dearmor -o /etc/apt/keyrings/docker.gpg
    chmod a+r /etc/apt/keyrings/docker.gpg
    echo \
      "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] \
      https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | \
      tee /etc/apt/sources.list.d/docker.list > /dev/null
    apt-get update -y
    apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin git
    systemctl enable --now docker
    usermod -aG docker ubuntu
}

if command -v docker &>/dev/null; then
    echo "==> Docker already installed, skipping."
else
    echo "==> Installing Docker..."
    case "$OS_ID" in
        amzn|al2023) install_docker_amazon_linux ;;
        ubuntu|debian) install_docker_ubuntu ;;
        *)
            echo "Unsupported OS: $OS_ID. Install Docker manually, then re-run."
            exit 1
            ;;
    esac
fi

# ── 3. Install Docker Compose plugin (if not bundled) ───────────────
if ! docker compose version &>/dev/null; then
    echo "==> Installing Docker Compose plugin..."
    COMPOSE_VERSION="v2.32.4"
    mkdir -p /usr/local/lib/docker/cli-plugins
    curl -SL "https://github.com/docker/compose/releases/download/${COMPOSE_VERSION}/docker-compose-$(uname -s)-$(uname -m)" \
        -o /usr/local/lib/docker/cli-plugins/docker-compose
    chmod +x /usr/local/lib/docker/cli-plugins/docker-compose
fi

echo "==> Docker Compose: $(docker compose version)"

# ── 4. Configure swap (useful for small instances) ──────────────────
if [ ! -f /swapfile ]; then
    echo "==> Creating 2 GB swap file..."
    dd if=/dev/zero of=/swapfile bs=1M count=2048
    chmod 600 /swapfile
    mkswap /swapfile
    swapon /swapfile
    echo '/swapfile none swap sw 0 0' >> /etc/fstab
fi

echo ""
echo "================================================================"
echo "  EC2 setup complete."
echo ""
echo "  Next steps:"
echo "    1. Clone your repo into /opt/alm-web (or your preferred path)"
echo "    2. Run the certificate init script:"
echo ""
echo "       cd /opt/alm-web"
echo "       sudo ./init-letsencrypt.sh your-domain.com you@email.com"
echo ""
echo "  This will obtain a Let's Encrypt certificate and start the app."
echo "================================================================"
