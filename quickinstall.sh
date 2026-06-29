echo "Setting up dependencies..."

source sourceRequirements.sh

echo "Building Vampire + VampLean..."

make build-deps

echo "Building VaLeaDate..."

make native

echo "Done! Test it using ./valeadate <problem_file>"