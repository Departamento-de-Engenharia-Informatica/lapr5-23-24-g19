export interface IFloorPersistence {
    domainId: string
    buildingCode: string
    floorNumber: number
    description: string
    map?:{
        dimensions:{
            mapLength: number
            mapWidth: number
        }
        
        mapContent: number[][]
        passages?: {
            x:number,
            y:number
        }[]
        rooms?: {
            x:number,
            y:number
        }[]
        elevators?: {
            x:number,
            y:number
        }[]
    }
}

