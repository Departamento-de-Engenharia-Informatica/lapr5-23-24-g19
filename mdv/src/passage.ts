import { Loader } from './loader'
import { MapFile } from './maze'

export type PassageParams = {
    cellCoords: { x: number; y: number }
    buildingA: { building: string; floor: number }
    buildingB: { building: string; floor: number }
}

export default class Passage {
    public cellCoordsB: { x: number; y: number }

    get buildingA() {
        return this.params.buildingA
    }

    get buildingB() {
        return this.params.buildingB
    }

    get cellCoords() {
        return this.params.cellCoords
    }

    constructor(
        private params: PassageParams,
        private loader: Loader,
    ) {
        this.fetchMap(
            `${import.meta.env.VITE_MDR_URL}/buildings/${
                params.buildingB.building
            }/floors/${params.buildingB.floor}/map`,
        )
    }

    private async fetchMap(url: string) {
        const description = await this.loader.load<MapFile>(url)

        const pas = description.map.passages.filter((p) => {
            return (
                p.to.building === this.buildingA.building &&
                p.to.floor === this.buildingA.floor
            )
        })

        this.cellCoordsB = {
            x: pas[0].x,
            y: pas[0].y,
        }
    }
}
