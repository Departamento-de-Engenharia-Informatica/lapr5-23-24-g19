export interface IRoomDTO {
    name: string
    buildingCode: string
    floorNumber: number
    description: string
    category: string
    dimensions: { length: number; width: number }
    positions: { x: number; y: number }
}
