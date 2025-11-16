import { Navigate, Route, Routes } from "react-router-dom";
import AppShell from "./components/layout/AppShell";
import TimelinePage from "./pages/TimelinePage";
import ArrowsPage from "./pages/ArrowsPage";
import OperatorsPage from "./pages/OperatorsPage";
import SettingsPage from "./pages/SettingsPage";
import IdentitiesPage from "./pages/IdentitiesPage";
import IdentityProfilePage from "./pages/IdentityProfilePage";

function App() {
  return (
    <AppShell>
      <Routes>
        <Route path="/" element={<TimelinePage />} />
        <Route path="/arrows" element={<ArrowsPage />} />
        <Route path="/operators" element={<OperatorsPage />} />
        <Route path="/identities" element={<IdentitiesPage />} />
        <Route path="/identities/:id" element={<IdentityProfilePage />} />
        <Route path="/settings" element={<SettingsPage />} />
        <Route path="*" element={<Navigate to="/" replace />} />
      </Routes>
    </AppShell>
  );
}

export default App;

