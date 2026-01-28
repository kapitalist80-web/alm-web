#!/usr/bin/env bash
# -------------------------------------------------------------------
# init-letsencrypt.sh – Obtain the first Let's Encrypt certificate
# and start the full stack (app + nginx + certbot renewal).
#
# Usage:
#   sudo ./init-letsencrypt.sh <domain> <email>
#
# Example:
#   sudo ./init-letsencrypt.sh alm.example.com admin@example.com
#
# What it does:
#   1. Starts nginx in HTTP-only mode (for the ACME challenge)
#   2. Runs certbot to obtain the certificate
#   3. Restarts nginx with the full HTTPS config
#   4. Sets up automatic certificate renewal via certbot
# -------------------------------------------------------------------
set -euo pipefail

DOMAIN="${1:?Usage: $0 <domain> <email>}"
EMAIL="${2:?Usage: $0 <domain> <email>}"
COMPOSE="docker compose"

echo "==> Domain: $DOMAIN"
echo "==> Email:  $EMAIL"

# Export DOMAIN so docker-compose.yml can use it
export DOMAIN

# ── 1. Start nginx with HTTP-only config for ACME challenge ─────────
echo "==> Starting nginx in HTTP-only mode..."
# Temporarily swap the nginx config to the init version
$COMPOSE down --remove-orphans 2>/dev/null || true

# Override nginx volumes to use init config
$COMPOSE run -d --name nginx-init \
    -p 80:80 \
    -v "$(pwd)/nginx/init-http.conf:/etc/nginx/conf.d/default.conf:ro" \
    -v "certbot-www:/var/www/certbot:ro" \
    nginx nginx -g "daemon off;"

echo "==> Waiting for nginx to start..."
sleep 3

# ── 2. Request certificate from Let's Encrypt ──────────────────────
echo "==> Requesting Let's Encrypt certificate..."
$COMPOSE run --rm certbot certonly \
    --webroot \
    --webroot-path=/var/www/certbot \
    --email "$EMAIL" \
    --agree-tos \
    --no-eff-email \
    -d "$DOMAIN"

# ── 3. Stop temporary nginx ─────────────────────────────────────────
echo "==> Stopping temporary nginx..."
docker stop nginx-init && docker rm nginx-init 2>/dev/null || true

# ── 4. Start the full stack with HTTPS ──────────────────────────────
echo "==> Starting full stack with HTTPS..."
$COMPOSE up -d

# ── 5. Set up automatic certificate renewal ─────────────────────────
echo "==> Setting up certificate renewal cron job..."
CRON_CMD="0 3 * * * cd $(pwd) && DOMAIN=$DOMAIN $COMPOSE run --rm certbot renew --quiet && $COMPOSE exec nginx nginx -s reload"

# Add cron job if not already present
(crontab -l 2>/dev/null | grep -v "certbot renew" || true; echo "$CRON_CMD") | crontab -

echo ""
echo "================================================================"
echo "  HTTPS setup complete!"
echo ""
echo "  Your app is now available at:"
echo "    https://$DOMAIN"
echo ""
echo "  Certificate auto-renewal is configured (daily at 03:00)."
echo "  Certificates renew automatically when < 30 days remain."
echo ""
echo "  Useful commands:"
echo "    docker compose logs -f          # View logs"
echo "    docker compose restart           # Restart all services"
echo "    docker compose down              # Stop all services"
echo "    docker compose up -d --build     # Rebuild and restart"
echo "================================================================"
