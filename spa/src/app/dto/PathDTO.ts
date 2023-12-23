export type PathDTO = PathSegmentDTO[]

export type PathSegmentDTO = CellSegmentDTO | ElevatorSegmentDTO | PassageSegmentDTO

interface Segment {
    type: 'cell' | 'elevator' | 'passage'
}

export type CellSegmentDTO = Segment & {
    building: string
    floor: number
    x: number
    y: number
}

export type ElevatorSegmentDTO = Segment & {
    building: string
    fromFloor: number
    toFloor: number
}

export type PassageSegmentDTO = Segment & {
    frombuilding: string
    fromfloor: number
    tobuilding: string
    tofloor: number
}
