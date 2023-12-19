export interface ICreateDeliveryTaskToMapperDTO {
    email: string

    startBuildingCode: string
    startFloorNumber: number
    startRoom: {
        x: number
        y: number
    }

    goalBuildingCode: string
    goalFloorNumber: number
    goalRoom: {
        x: number
        y: number
    }

    pickupContactName: string
    pickupContactPhone: string

    deliveryContactName: string
    deliveryContactPhone: string

    description?: string
    confirmationCode: number
}
