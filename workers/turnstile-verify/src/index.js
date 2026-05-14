const ALLOWED_ORIGIN = "https://follhim.net";
const CV_URL = "https://bitli.pro/2KRCT_c18bc349";

const corsHeaders = {
  "Access-Control-Allow-Origin": ALLOWED_ORIGIN,
  "Access-Control-Allow-Methods": "POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type",
};

export default {
  async fetch(request, env) {
    // Handle CORS preflight
    if (request.method === "OPTIONS") {
      return new Response(null, { status: 204, headers: corsHeaders });
    }

    if (request.method !== "POST") {
      return new Response("Method Not Allowed", { status: 405 });
    }

    // Parse request body
    let token;
    try {
      const body = await request.json();
      token = body.token;
    } catch {
      return new Response(
        JSON.stringify({ success: false, error: "Invalid JSON body" }),
        { status: 400, headers: { "Content-Type": "application/json", ...corsHeaders } }
      );
    }

    if (!token) {
      return new Response(
        JSON.stringify({ success: false, error: "Missing token" }),
        { status: 400, headers: { "Content-Type": "application/json", ...corsHeaders } }
      );
    }

    // Validate token with Cloudflare Siteverify
    const formData = new FormData();
    formData.append("secret", env.TURNSTILE_SECRET);
    formData.append("response", token);
    formData.append("remoteip", request.headers.get("CF-Connecting-IP") ?? "");

    try {
      const verifyRes = await fetch(
        "https://challenges.cloudflare.com/turnstile/v0/siteverify",
        { method: "POST", body: formData }
      );
      const outcome = await verifyRes.json();

      return new Response(
        JSON.stringify({
          success: outcome.success,
          url: outcome.success ? CV_URL : null
        }),
        { headers: { "Content-Type": "application/json", ...corsHeaders } }
      );
    } catch (err) {
      return new Response(
        JSON.stringify({ success: false, error: "Siteverify request failed" }),
        { status: 502, headers: { "Content-Type": "application/json", ...corsHeaders } }
      );
    }
  },
};
