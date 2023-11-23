export interface RoomDTO {
    name: string
    description: string
    category: string
    dimensions: { length: number; width: number }
    positions: { x: number; y: number }
}
