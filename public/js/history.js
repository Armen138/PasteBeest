var pastes = {
    pastes: {},
    add: function(id) {
        pastes.pastes[id] = {
            timestamp: Date.now(),
            language: language
        };
        pastes.save();
    },
    save: function() {
        localStorage.history = JSON.stringify(pastes.pastes);
    },
    load: function() {
        if(localStorage.history) {
            pastes.pastes = JSON.parse(localStorage.history);
        }
    }
};

pastes.load();

for(var nom in pastes) {
    exports[nom] = pastes[nom];
}
// exports = pastes;