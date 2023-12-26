export interface IGeneralTaskDTO {
    email: string
    location: {
        startingPoint: {
            buildingCode: string
            floorNumber: number
            x: number
            y: number
        }
        endingPoint: {
            buildingCode: string
            floorNumber: number
            x: number
            y: number
        }
    }

    status: number
    jobType: number

    id: {
        value: string
    }

    surveillanceContact?: {
        name: string
        phoneNumber: string
    }

    pickupContact?: {
        name: string
        phoneNumber: string
    }
    deliveryContact?: {
        name: string
        phoneNumber: string
    }
    confirmationCode?: {
        code: number
    }
    description?: string
}
