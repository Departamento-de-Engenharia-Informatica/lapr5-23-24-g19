export interface CreateDeliveryTaskDTO {
    email: string

    startBuildingCode: string
    startFloorNumber: number
    startRoom: string

    goalBuildingCode: string
    goalFloorNumber: number
    goalRoom: string

    pickupContactName: string
    pickupContactPhone: string

    deliveryContactName: string
    deliveryContactPhone: string

    description?: string
    confirmationCode: number
}
