import { Navigate, Route, Routes } from "react-router-dom";
import AppShell from "./components/layout/AppShell";
import LinearDashboardPage from "./pages/LinearDashboardPage";
import TimelinePage from "./pages/TimelinePage";
import SettingsPage from "./pages/SettingsPage";
import IdentitiesPage from "./pages/IdentitiesPage";
import IdentityProfilePage from "./pages/IdentityProfilePage";

function App() {
  return (
    <AppShell>
      <Routes>
        <Route path="/" element={<TimelinePage />} />
        <Route path="/linear" element={<LinearDashboardPage />} />
        <Route path="/identities" element={<IdentitiesPage />} />
        <Route path="/identities/:id" element={<IdentityProfilePage />} />
        <Route path="/settings" element={<SettingsPage />} />
        <Route path="*" element={<Navigate to="/" replace />} />
      </Routes>
    </AppShell>
  );
}

export default App;

