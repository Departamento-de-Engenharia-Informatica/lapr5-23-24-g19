describe('List Buildings with a min and max floors e2e tests', () => {
    beforeEach(() => {
        cy.visit('/campus/buildings/list-by-floors')
    })
    it('should have the correct title', () => {
        cy.title().should('equal', 'List Buildings by Floors')
    })

    it('should get buildings by entering min and max floor values', () => {
        const min = '0'
        const max = '2'

        const filteredBuildingsFloorsMockData = [
            {
                code: 'P',
                name: 'Physics',
                description: 'Physics Department',
                maxFloorDimensions: {
                    length: 26,
                    width: 12,
                },
                floorNumber: 1,
            },
            {
                code: 'C',
                name: 'Chemistry',
                description: 'Chemistry Department',
                maxFloorDimensions: {
                    length: 20,
                    width: 15,
                },
                floorNumber: 2,
            },
        ]

        cy.intercept(
            'GET',
            `http://localhost:4000/api/buildings/?minFloors=${min}&maxFloors=${max}`,
            {
                statusCode: 200,
                body: filteredBuildingsFloorsMockData,
            },
        ).as('getBuildingsFloors')

        cy.get('#min').type(min)
        cy.get('#max').type(max)
        cy.get('button[type="submit"]').click()

        cy.wait('@getBuildingsFloors')

        cy.get('.building-card').should(
            'have.length',
            filteredBuildingsFloorsMockData.length,
        )

        filteredBuildingsFloorsMockData.forEach((building, index) => {
            cy.get(`.building-card:eq(${index}) h2`).should('contain.text', building.code)
            cy.get(`.building-card:eq(${index}) p`).should(
                'contain.text',
                building.description,
            )
            cy.get(`.building-card:eq(${index}) .dimension-box:eq(0)`).should(
                'contain.text',
                `Length: ${building.maxFloorDimensions.length}`,
            )
            cy.get(`.building-card:eq(${index}) .dimension-box:eq(1)`).should(
                'contain.text',
                `Width: ${building.maxFloorDimensions.width}`,
            )
            cy.get(`.building-card:eq(${index}) .dimension-box:eq(2)`).should(
                'contain.text',
                `Floors: ${building.floorNumber}`,
            )
        })
    })

    it('should handle no buildings within the specified min and max floor range', () => {
        const min = '3'
        const max = '5'

        cy.intercept(
            'GET',
            `http://localhost:4000/api/buildings/?minFloors=${min}&maxFloors=${max}`,
            {
                statusCode: 200,
                body: [],
            },
        ).as('getBuildingsNoFloors')

        cy.get('#min').type(min)
        cy.get('#max').type(max)
        cy.get('button[type="submit"]').click()

        cy.wait('@getBuildingsNoFloors')

        cy.get('.building-card').should('not.exist')
    })

    it('should handle negative input for min and max floors', () => {
        let invalidMin = '-1'
        let invalidMax = '2'

        cy.get('#min').type(invalidMin)
        cy.get('#max').type(invalidMax)
        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('.building-card').should('not.exist')

        invalidMin = '1'
        invalidMax = '-2'

        cy.get('#min').type(invalidMin)
        cy.get('#max').type(invalidMax)
        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('.building-card').should('not.exist')

        invalidMin = '-1'
        invalidMax = '-2'

        cy.get('#min').type(invalidMin)
        cy.get('#max').type(invalidMax)
        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('.building-card').should('not.exist')
    })

    it('should handle input for higher min than max value', () => {
        let invalidMin = '5'
        let invalidMax = '2'

        cy.get('#min').type(invalidMin)
        cy.get('#max').type(invalidMax)
        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('.building-card').should('not.exist')
    })
})
