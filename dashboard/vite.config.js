var _a;
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
export default defineConfig({
    plugins: [react()],
    server: {
        port: 5173,
        host: true,
    },
    define: {
        __APP_VERSION__: JSON.stringify((_a = process.env.npm_package_version) !== null && _a !== void 0 ? _a : "dev"),
    },
});
