{
  pkgs,
  ...
}:
let
  ollamaEnable = true;
  ollamaPort = 11434;
  webuiEnable = false;
  webuiPort = 10000;
in
{
  nixpkgs.config.rocmSupport = ollamaEnable;

  services.ollama = {
    package = pkgs.ollama-rocm;
    enable = ollamaEnable;
    host = "0.0.0.0";
    port = ollamaPort;
    loadModels = [
      # "gemma2:9b"
      "qwen2.5-coder:7b" # coding / editor autocomplete (Q4_K_M, ~4.7 GB)
      "qwen3:8b" # general + agentic + tool-calling; fully in VRAM, roomy context
      "qwen3:30b" # MoE (30B total / ~3B active); VRAM+RAM split, bigger-model quality
      # "qwen2.5-coder:14b" # coding ceiling - Q4 only, tight context
    ];
    rocmOverrideGfx = "10.3.0";
    environmentVariables = {
      # Default context window for all models (agentic coding needs a big one).
      # Bigger num_ctx => bigger KV cache => more VRAM per loaded model.
      OLLAMA_CONTEXT_LENGTH = "32768";
    };
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      ollamaPort
      webuiPort
    ];
  };

  services.open-webui = {
    enable = webuiEnable;
    host = "0.0.0.0";
    port = webuiPort;
    environment = {
      WEBUI_AUTH = "False";
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";

      # 1. Disable Gravatar (Stops sending your email hash to Gravatar servers)
      ENABLE_GRAVATAR = "False";
      # 2. Disable Update Checks (Stops pinging GitHub/Home to check for new versions)
      CHECK_UPDATES = "False";
      # 3. Disable Community Sharing (Prevents accidental sharing of prompts/tools to the public site)
      ENABLE_COMMUNITY_SHARING = "False";
      # 4. Security: Disable New Signups (Crucial!)
      # Once you have recovered your account, set this to False.
      # This prevents anyone else on your network from creating an account on your LLM.
      ENABLE_SIGNUP = "False";

      OLLAMA_API_BASE_URL = "http://127.0.0.1:${toString ollamaPort}/api";
      OLLAMA_BASE_URL = "http://127.0.0.1:${toString ollamaPort}";
    };
  };
}
