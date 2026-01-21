# ============================================================================
# GOLDENTICKETCORE LOADER - SIMPLE VERSION
# ============================================================================
# Place at: ~/GitHub/NicheSportSimsPublic/CoreFunctions/load_core.R

# Hardcoded path - change if your structure is different
core_path <- path.expand("~/GitHub/NicheSportSimsPublic/CoreFunctions")

# Check if folder exists
if(!dir.exists(core_path)) {
  stop("\n❌ CoreFunctions folder not found!\n",
       "   Looking at: ", core_path, "\n",
       "   Current working directory: ", getwd(), "\n")
}

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║           LOADING GOLDENTICKETCORE                         ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
cat("Location:", core_path, "\n\n")

# Load files
source(file.path(core_path, "01_build_optimal_analysis.R"))
cat("  ✓ 01_build_optimal_analysis.R\n")

source(file.path(core_path, "02_find_optimal_lineups.R"))
cat("  ✓ 02_find_optimal_lineups.R\n")

source(file.path(core_path, "03_lp_optimization.R"))
cat("  ✓ 03_lp_optimization.R\n")

source(file.path(core_path, "04_score_all_sims.R"))
cat("  ✓ 04_score_all_sims.R\n")

source(file.path(core_path, "05_distribution_metrics.R"))
cat("  ✓ 05_distribution_metrics.R\n")

cat("\n✅ GoldenTicketCore loaded successfully!\n\n")