export type Passage = {
    x: number
    y: number
    orientation: string
    to: {
        building: string
        floor: number
    }
}

export type Room = {
    x: number
    y: number
    orientation: string
    name: string
}

export type Elevator = {
    x: number
    y: number
    orientation: 'N' | 'S' | 'W' | 'E'
    floors: number[]
}
