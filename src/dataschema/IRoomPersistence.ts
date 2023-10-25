export interface IRoomPersistence {
    domainId: string
    buildingCode: string

    floor: number
    name: string

    description: string

    category: string

    roomDimensionLength: number

    roomDimensionWidth: number

    roomPositionX: number

    roomPositionY: number
}
