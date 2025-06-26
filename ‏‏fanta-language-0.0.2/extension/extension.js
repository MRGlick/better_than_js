const vscode = require('vscode');
const diagnostics = require('./src/diagnostics');

function activate(context) {
    diagnostics.activate(context);

    // Register a completion item provider for the "fanta" language
    const provider = vscode.languages.registerCompletionItemProvider(
        { scheme: 'file', language: 'fanta' },
        {
            provideCompletionItems(document, position) {
                const text = document.getText();
                const functionRegex = /\b(?:int|float|string|bool|void)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(/g;

                const suggestions = [];
                let match;

                while ((match = functionRegex.exec(text)) !== null) {
                    const functionName = match[1];
                    const item = new vscode.CompletionItem(functionName, vscode.CompletionItemKind.Function);
                    item.insertText = new vscode.SnippetString(`${functionName}($1)`);
                    item.documentation = `Call to function ${functionName}`;
                    suggestions.push(item);
                }

                return suggestions;
            }
        },
        '.' // Trigger completion after typing any character like ".", "(", etc.
    );

    context.subscriptions.push(provider);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
};
