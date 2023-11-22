describe('Floors e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', `http://localhost:4000/status`, {
            statusCode: 200,
        }).as('getBuildings')

        cy.intercept('GET', `http://localhost:4000/api/buildings`, {
            body: [
                {
                    code: 'P',
                    name: 'Physics',
                    description: 'Physics Department',
                    maxFloorDimensions: {
                        length: 26,
                        width: 12,
                    },
                },
                {
                    code: 'C',
                    name: 'Chemistry',
                    description: 'Chemistry Department',
                    maxFloorDimensions: {
                        length: 20,
                        width: 15,
                    },
                },
            ],
        }).as('getBuildings')

        cy.visit('/floors')
    })

    describe('List floors e2e tests', () => {
        beforeEach(() => {
            cy.visit('/floors/list')
        })

        it('has the correct title', () => {
            cy.title().should('equal', 'List Floors')
        })

        it('should have an empty selected building', () => {
            cy.get('#building').should('have.value', null)
        })

        it('should initially have an empty floor list', () => {
            cy.get('.floor-card').should('not.exist')
        })

        it('should select a building and display floors', () => {
            cy.wait('@getBuildings')
            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: 'Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('#building').select('P')

            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', 'Floor 2')
            cy.get('.floor-card').should('contain.text', 'Physics Labs')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
                body: [
                    {
                        buildingCode: 'C',
                        floorNumber: 1,
                        description: 'Chemistry Labs',
                    },
                    {
                        buildingCode: 'C',
                        floorNumber: 2,
                        description: 'Chemistry Research',
                    },
                ],
            }).as('getFloorsChemistry')

            cy.get('#building').select('C')
            cy.wait('@getFloorsChemistry')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', 'Floor 1')
            cy.get('.floor-card').should('contain.text', 'Chemistry Labs')
            cy.get('.floor-card').should('contain.text', 'Floor 2')
            cy.get('.floor-card').should('contain.text', 'Chemistry Research')
        })

        it('should handle floors with empty descriptions', () => {
            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: '',
                    },
                ],
            }).as('getFloorsEmptyDescription')

            cy.get('#building').select('P')

            cy.wait('@getFloorsEmptyDescription')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', 'No description')
        })
    })

    describe('Create Floors e2e tests', () => {
        beforeEach(() => {
            cy.visit('/floors/create')
        })

        it('should have the correct title', () => {
            cy.title().should('equal', 'Create Floor')
        })

        it('should display the form elements', () => {
            cy.get('#selectedBuilding').should('exist')
            cy.get('#floorNumber').should('exist')
            cy.get('#description').should('exist')
            cy.get('button[type="submit"]').should('exist')
        })

        it('should display a list of buildings in the select dropdown', () => {
            cy.get('#selectedBuilding').select('P').should('have.value', 'P')
            cy.get('#selectedBuilding').select('C').should('have.value', 'C')
        })

        it('should display existing floors when a building is selected', () => {
            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Physics Labs',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: 'Physics Research',
                    },
                ],
            }).as('getExistingFloors')

            cy.get('#selectedBuilding').select('P')
            cy.wait('@getExistingFloors')

            cy.get('.floor-card').should('have.length', 2)
            cy.get('.floor-card').should('contain.text', 'Floor 1')
            cy.get('.floor-card').should('contain.text', 'Physics Labs')
            cy.get('.floor-card').should('contain.text', 'Floor 2')
            cy.get('.floor-card').should('contain.text', 'Physics Research')
        })

        it('should be able to create a new floor', () => {
            const floorNumber = '3'
            const description = 'New Floor'

            cy.get('#selectedBuilding').select('P')
            cy.get('#floorNumber').type(floorNumber)
            cy.get('#description').type(description)

            cy.intercept('POST', 'http://localhost:4000/api/buildings/P/floors', {
                statusCode: 201,
                body: {
                    buildingCode: 'P',
                    floorNumber: floorNumber,
                    description: description,
                },
            }).as('createFloor')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: floorNumber,
                        description: description,
                    },
                ],
            }).as('getFloors')

            cy.get('button[type="submit"]').click()
            cy.wait('@createFloor')
            cy.wait('@getFloors')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', `Floor ${floorNumber}`)
            cy.get('.floor-card').should('contain.text', description)
        })

        it('should handle form validation', () => {
            const floorNumber = '3'
            const description = 'New Floor'

            cy.get('button[type="submit"]').should('be.disabled')

            cy.get('#selectedBuilding').should('have.class', 'ng-invalid')
            cy.get('#floorNumber').should('have.class', 'ng-invalid')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: floorNumber,
                        description: description,
                    },
                ],
            }).as('getFloors')

            cy.get('#selectedBuilding').select('P')
            cy.wait('@getFloors')
            cy.get('button[type="submit"]').should('be.disabled')

            cy.get('#description').type(
                'This is optional but the form should still be disabled without typing the floor number',
            )
            cy.get('button[type="submit"]').should('be.disabled')

            cy.get('#floorNumber').type('3')
            cy.get('button[type="submit"]').should('not.be.disabled')
        })

        it('should reset the form after floor creation', () => {
            const floorNumber = '3'
            const description = 'New Floor'

            cy.get('#selectedBuilding').select('P')
            cy.get('#floorNumber').type(floorNumber)
            cy.get('#description').type(description)

            cy.intercept('POST', 'http://localhost:4000/api/buildings/P/floors', {
                statusCode: 201,
                body: {
                    buildingCode: 'P',
                    floorNumber: floorNumber,
                    description: description,
                },
            }).as('createFloor')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: floorNumber,
                        description: description,
                    },
                ],
            }).as('getFloors')

            cy.get('button[type="submit"]').click()
            cy.wait('@createFloor')
            cy.wait('@getFloors')

            cy.get('#selectedBuilding').should('have.value', 'P')
            cy.get('#floorNumber').should('have.value', '')
            cy.get('#description').should('have.value', '')
        })
    })
})
