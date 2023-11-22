describe('CreatePassageComponent', () => {

	beforeEach(() => {
		cy.intercept('GET', 'http://localhost:4000/api/buildings', {
			body: [
				{ code: 'P', name: 'Physics' },
				{ code: 'C', name: 'Chemistry' },
			],
		}).as('getBuildings');

		cy.visit('campus/passages/create');
	});

	it('should display the form with building options', () => {
		cy.wait('@getBuildings')

		// Check if the form is present
		cy.get('#passageForm').should('exist');
		cy.get('#building1').should('exist');
		cy.get('#floor1').should('exist');
		cy.get('#building2').should('exist');
		cy.get('#floor2').should('exist');
		cy.get('button').should('exist');
	});

	it('should submit the form with valid input', () => {
		cy.wait('@getBuildings')

		cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
			body: [
				{ floorNumber: 1 },
				{ floorNumber: 2 },
			],
		}).as('getFloorsPhysics');

		cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
			body: [
				{ floorNumber: 1 },
				{ floorNumber: 2 },
			],
		}).as('getFloorsChemistry');

		// Select building and floor options
		cy.get('#building1').select('P');
		cy.wait('@getFloorsPhysics');
		cy.get('#floor1').select('1');

		cy.get('#building2').select('C');
		cy.wait('@getFloorsChemistry');
		cy.get('#floor2').select('2');


		cy.intercept('POST', 'http://localhost:4000/api/passages', {
			statusCode: 200,
			body: { success: true },
		}).as('postPassage');

		// Submit the form
		cy.get('#submitPassage').click();
		cy.wait('@postPassage').its('response.statusCode').should('eq', 200);
		//TODO: Check success message

	});


	it('should show error form with invalid input', () => {
		cy.wait('@getBuildings')

		cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
			body: [
				{ floorNumber: 1 },
				{ floorNumber: 2 },
			],
		}).as('getFloorsPhysics');

		cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
			body: [
				{ floorNumber: 1 },
				{ floorNumber: 2 },
			],
		}).as('getFloorsChemistry');

		cy.get('#building1').select('P');
		cy.wait('@getFloorsPhysics');
		cy.get('#floor1').select('1');

		cy.get('#building2').select('C');
		cy.wait('@getFloorsChemistry');
		cy.get('#floor2').select('2');

		cy.intercept('POST', 'http://localhost:4000/api/passages', {
			statusCode: 422,
			body: { success: true },
		}).as('postPassage');

		cy.get('#submitPassage').click();

		cy.wait('@postPassage').its('response.statusCode').should('eq', 422);
		//TODO: ensure error message is returned
	});


	it('should show error when building does not have floor', () => {
		cy.wait('@getBuildings')

		cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
			body: [
				{ floorNumber: 1 },
				{ floorNumber: 2 },
			],
		}).as('getFloorsPhysics');

		cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
			body: [],
		}).as('getFloorsChemistry');

		// Select building and floor options
		cy.get('#building1').select('P');
		cy.wait('@getFloorsPhysics');
		cy.get('#floor1').select('1');

		cy.get('#building2').select('C');
		cy.wait('@getFloorsChemistry');
		cy.get('#floor2').should('contain.text','No floors found');

	});
});
