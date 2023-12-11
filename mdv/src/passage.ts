export type PassageParams = {
    cellCoords: { x: number; y: number }
    buildingA: { building: string; floor: number }
    buildingB: { building: string; floor: number }
}

export default class Passage {
    get buildingA() {
        return this.params.buildingA
    }

    get buildingB() {
        return this.params.buildingB
    }

    get cellCoords() {
        return this.params.cellCoords
    }

    constructor(private params: PassageParams) {}
}
