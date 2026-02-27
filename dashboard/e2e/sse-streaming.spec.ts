import { test, expect } from "@playwright/test";

// Both the API server (port 8485) and Vite dev server (port 34853) must be
// running behind Caddy at http://vibeos.localhost for these tests to pass.
// The Caddyfile must include `flush_interval -1` for SSE streaming.

test.describe("SSE streaming – timeline events", () => {
  test("timeline loads threads via SSE stream", async ({ page }) => {
    await page.goto("/");

    // Wait for the summary to show a non-zero entry count.
    // The list is reactive to the data -- entries appear as they stream in.
    const summary = page.locator("text=/Showing [1-9]\\d* entries between/");
    await expect(summary).toBeVisible({ timeout: 45_000 });

    // No SSE error should be visible.
    await expect(page.locator("text=SSE connection error")).toBeHidden();
  });

  test("no SSE error messages are displayed", async ({ page }) => {
    await page.goto("/");

    await expect(
      page.locator("text=/Showing [1-9]\\d* entries between/"),
    ).toBeVisible({ timeout: 45_000 });

    // Assert no SSE errors anywhere on the page.
    await expect(page.locator("text=SSE connection error for events stream")).toHaveCount(0);
    await expect(page.locator("text=SSE connection error for search stream")).toHaveCount(0);
  });
});

test.describe("SSE streaming – search", () => {
  test("search streams results progressively", async ({ page }) => {
    await page.goto("/");

    await expect(
      page.locator("text=/Showing [1-9]\\d* entries between/"),
    ).toBeVisible({ timeout: 45_000 });

    const searchInput = page.getByPlaceholder("Search timeline...");
    await searchInput.fill("the");
    await page.getByRole("button", { name: "Search" }).click();

    // Wait for search results with a non-zero total (the done event sets total).
    const resultsLine = page.locator("text=/Found [1-9]\\d* results for/");
    await expect(resultsLine).toBeVisible({ timeout: 30_000 });

    // No search SSE error.
    await expect(page.locator("text=SSE connection error for search stream")).toHaveCount(0);
  });

  test("clearing search returns to timeline view", async ({ page }) => {
    await page.goto("/");

    await expect(
      page.locator("text=/Showing \\d+ entries between/"),
    ).toBeVisible({ timeout: 30_000 });

    // Search for something.
    const searchInput = page.getByPlaceholder("Search timeline...");
    await searchInput.fill("test");
    await page.getByRole("button", { name: "Search" }).click();

    // Wait for search mode.
    await expect(page.locator("text=/Found \\d+ results for/")).toBeVisible({
      timeout: 30_000,
    });

    // Clear the search.
    await page.getByRole("button", { name: "Clear" }).click();

    // Should return to timeline summary.
    await expect(
      page.locator("text=/Showing [1-9]\\d* entries between/"),
    ).toBeVisible({ timeout: 45_000 });
  });
});
