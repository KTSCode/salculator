# Salculator
Really Simple app that calculates tithe amounts based on salary inputs

[Salculator](https://ktscode.github.io/salculator)

## Development Setup

### Prerequisites
- [Elm](https://elm-lang.org/)
- [fswatch](https://emcrisostomo.github.io/fswatch/)
- Tailwind CSS CLI (binary)

### Instructions
1. **Install fswatch** (if not already installed):
   ```sh
   brew install fswatch
   ```

2. **Start Tailwind CSS in watch mode**:
   Run the following command to watch for changes in the `src/tailwind.css` file and update the `main.css` file:
   ```sh
   ./tailwindcss -i ./src/tailwind.css -o ./main.css --watch
   ```

3. **Start Elm in watch mode**:
   Run the following command to watch for changes in the `src/Main.elm` file and recompile it to `main.js`:
   ```sh
   fswatch -o src/Main.elm | xargs -n1 -I{} elm make src/Main.elm --output=main.js
   ```

4. **Open index.html**:
   After running the above commands, open `index.html` to see the changes:
   ```sh
   open index.html
   ```

These commands will ensure that your CSS and Elm files are watched for changes, and the respective output files are updated automatically.

### Deployment
Compile the Elm code to HTML and JS

```sh
elm make src/Main.elm --optimize --output=main.js
./tailwindcss -i ./src/tailwind.css -o ./main.css
```
