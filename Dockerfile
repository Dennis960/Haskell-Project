# --- Builder Stage ---
FROM alpine:latest AS builder

ENV LANG C.UTF-8

# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="false"

# Install necessary packages
RUN apk add --no-cache \
    bash \
    curl \
    gcc \
    musl-dev \
    libffi-dev \
    gmp-dev \
    numactl-dev \
    ncurses-dev \
    zlib-dev \
    zsh \
    make \
    libc-dev \
    linux-headers \
    git

# Setup non-root user
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN addgroup -g $USER_GID $USERNAME \
    && adduser -D -u $USER_UID -G $USERNAME $USERNAME

# Install latest GHCup in the non-root user home
USER $USERNAME

RUN mkdir -p "$HOME/.ghcup/bin" \
    && curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "$HOME/.ghcup/bin/ghcup" \
    && chmod +x "$HOME/.ghcup/bin/ghcup"

ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"

# [Choice] GHC version: recommended, latest, 9.2, 9.0, 8.10, 8.8, 8.6
ARG GHC_VERSION="9.2.8"

# Attempt to install GHC and other tools
RUN ghcup install ghc "${GHC_VERSION}" --set \
    && ghcup install cabal recommended --set \
    && ghcup install stack recommended --set \
    && ghcup install hls recommended --set \
    && cabal update

# Setting up project for building
WORKDIR /haskell-project
COPY src /haskell-project/src

# Build the project
RUN mkdir -p dist \
    && ghc -o dist/main -isrc src/Main.hs -outputdir build -optl-pthread \
    && cp -r src/rooms dist/.

USER root
# --- Final Stage ---
FROM alpine

# Install runtime dependencies
RUN apk add --no-cache gmp

ENV LANG=C.UTF-8

WORKDIR /haskell-project/dist

# Copy only the built binary and needed files from builder
COPY --from=builder /haskell-project/dist /haskell-project/dist

CMD ["./main"]
