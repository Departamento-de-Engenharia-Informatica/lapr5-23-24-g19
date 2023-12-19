export interface IRoomPersistence {
    domainId: string
    name: string
    buildingCode: string
    floorNumber: number
    description: string
    category: string

    dimensions: {
        length: number
        width: number
    }
    position: {
        x: number
        y: number
    }
}
