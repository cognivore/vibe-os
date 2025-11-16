import { Link, useLocation } from "react-router-dom";
import { ReactNode } from "react";
import { cn } from "../../lib/utils";

const NAV_ITEMS = [
  { label: "Timeline", to: "/" },
  { label: "Arrows", to: "/arrows" },
  { label: "Operators", to: "/operators" },
  { label: "Identities", to: "/identities" },
  { label: "Settings", to: "/settings" },
];

interface AppShellProps {
  children: ReactNode;
}

export default function AppShell({ children }: AppShellProps) {
  const location = useLocation();
  return (
    <div className="min-h-screen bg-background text-foreground">
      <div className="flex min-h-screen">
        <aside className="hidden w-64 border-r border-border bg-card/40 p-6 sm:block">
          <div className="mb-6">
            <p className="text-xs uppercase tracking-wide text-muted-foreground">
              Vibe OS
            </p>
            <h1 className="text-2xl font-semibold">Event Dashboard</h1>
          </div>
          <nav className="space-y-2 text-sm">
            {NAV_ITEMS.map((item) => {
              const active = location.pathname === item.to;
              return (
                <Link
                  key={item.to}
                  to={item.to}
                  className={cn(
                    "block rounded-md px-3 py-2 transition-colors",
                    active
                      ? "bg-primary text-primary-foreground"
                      : "text-muted-foreground hover:bg-muted hover:text-foreground",
                  )}
                >
                  {item.label}
                </Link>
              );
            })}
          </nav>
        </aside>
        <main className="flex-1">
          <header className="border-b border-border bg-background/80 backdrop-blur">
            <div className="mx-auto flex max-w-6xl items-center justify-between px-6 py-4">
              <div>
                <p className="text-sm text-muted-foreground">
                  Unified events, operators, and arrows
                </p>
                <p className="text-xs text-muted-foreground">
                  API target: {import.meta.env.VITE_API_BASE ?? "http://localhost:3000"}
                </p>
              </div>
            </div>
          </header>
          <div className="mx-auto max-w-6xl px-4 py-6">{children}</div>
        </main>
      </div>
    </div>
  );
}

