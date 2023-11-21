import { TestBed } from '@angular/core/testing'

import { ElevatorService } from './elevator.service'

describe('ElevatorService', () => {
    let service: ElevatorService

    beforeEach(() => {
        TestBed.configureTestingModule({})
        service = TestBed.inject(ElevatorService)
    })

    it('should be created', () => {
        expect(service).toBeTruthy()
    })
})
