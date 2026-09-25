import { build } from "esbuild";
import { copyFile, mkdir, readFile, readdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

const manifest = JSON.parse(await readFile("package.json", "utf8"));
if (!/^0\.0\.[0-9]+([.-][a-z0-9.]+)?$/.test(manifest.version)) {
  throw new Error("Release versions must remain on the 0.0.x line");
}

await mkdir("dist", { recursive: true });
const common = {
  entryPoints: ["output/LayeredLayout.Npm/index.js"],
  bundle: true,
  platform: "neutral",
  target: "es2020",
  legalComments: "eof",
  minify: true,
  banner: {
    js: "/*! @markgrafhq/layered-layout | MIT AND EPL-2.0 | See LICENSE, LICENSE-EPL-2.0 and THIRD-PARTY-NOTICES.txt. */",
  },
};
await Promise.all([
  build({ ...common, format: "esm", outfile: "dist/index.js" }),
  build({ ...common, format: "cjs", outfile: "dist/index.cjs" }),
  copyFile("npm/index.d.ts", "dist/index.d.ts"),
  copyFile("npm/index.d.ts", "dist/index.d.cts"),
]);

// PureScript dependencies are linked into the bundles, not installed by consumers.
// Ship their license texts alongside the original EPL-covered sources in src/.
const notices = [];
const lock = JSON.parse(await readFile("spago.lock", "utf8"));
async function collect(directory, label) {
  for (const entry of (await readdir(directory, { withFileTypes: true })).sort((a, b) => a.name.localeCompare(b.name))) {
    const path = join(directory, entry.name);
    const name = `${label}/${entry.name}`;
    if (entry.isDirectory()) await collect(path, name);
    else notices.push(`===== ${name} =====\n\n${await readFile(path, "utf8")}`);
  }
}
for (const [name, dependency] of Object.entries(lock.packages).sort(([a], [b]) => a.localeCompare(b))) {
  const label = `${name}-${dependency.version}`;
  const directory = join(".spago/p", label);
  for (const entry of (await readdir(directory, { withFileTypes: true })).sort((a, b) => a.name.localeCompare(b.name))) {
    if (!/^licen[cs]e(?:[._-].*)?$/i.test(entry.name)) continue;
    const path = join(directory, entry.name);
    if (entry.isDirectory()) await collect(path, `${label}/${entry.name}`);
    else notices.push(`===== ${label}/${entry.name} =====\n\n${await readFile(path, "utf8")}`);
  }
}
if (notices.length === 0) throw new Error("No bundled dependency licenses found");
await writeFile("dist/THIRD-PARTY-NOTICES.txt", notices.join("\n\n"));
