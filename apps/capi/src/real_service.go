package services

import (
	"encoding/base64"
	middleware "github.com/go-openapi/runtime/middleware"
	config "github.com/rbkmoney/common_api/config"
	"github.com/rbkmoney/common_api/models"
	base "github.com/rbkmoney/common_api/proto/base"
	cds "github.com/rbkmoney/common_api/proto/cds"
	domain "github.com/rbkmoney/common_api/proto/domain"
	payment_processing "github.com/rbkmoney/common_api/proto/payment_processing"
	auth "github.com/rbkmoney/common_api/restapi/auth"
	"github.com/rbkmoney/common_api/restapi/operations/invoices"
	"github.com/rbkmoney/common_api/restapi/operations/payments"
	"github.com/rbkmoney/common_api/restapi/operations/tokens"
	client "github.com/rbkmoney/woody_go/woody/client"
	context "github.com/rbkmoney/woody_go/woody/context"
	event "github.com/rbkmoney/woody_go/woody/event"

	"encoding/json"
	"github.com/Sirupsen/logrus"
	"strconv"
	"strings"
	"time"
)

type realCommonAPIService struct {
	config config.Config
}

func newRealCommonAPIService() CommonAPIService {
	return new(realCommonAPIService)
}

func (s *realCommonAPIService) Start(config config.Config) error {
	s.config = config
	return nil
}

func (s *realCommonAPIService) CreateInvoice(params invoices.CreateInvoiceParams, userContext auth.Context, loggerContext *logrus.Entry) middleware.Responder {
	c := s.createInvoicingClient(params.XRequestID, loggerContext)
	incomingArgs := params.CreateInvoiceArgs

	var userInfo *payment_processing.UserInfo
	var invoiceParams *payment_processing.InvoiceParams
	userInfo = getUserInfo(userContext)

	var description string
	description = encodeDescription(incomingArgs.Description)

	currency := encodeCurrency(incomingArgs.Currency)
	invoiceParams = &payment_processing.InvoiceParams{
		Product:     encodeProduct(incomingArgs.Product),
		Description: &description,
		Due:         encodeDateTime(incomingArgs.DueDate),
		Amount:      encodeAmount(incomingArgs.Amount),
		Currency:    &currency,
		Context:     encodeInvoiceContext(incomingArgs.Context),
	}

	var domainInvoiceID domain.InvoiceID
	domainInvoiceID, err := c.Create(userInfo, invoiceParams)
	if err != nil {
		loggerContext.Error("Wrong response: ", err.Error())
		panic(err)
	}

	id := decodeInvoiceID(domainInvoiceID)
	payload := invoices.CreateInvoiceCreatedBodyBody{
		ID: &id,
	}

	loggerContext.Info("success")
	return invoices.NewCreateInvoiceCreated().WithPayload(payload)
}

func (s *realCommonAPIService) GetInvoiceByID(params invoices.GetInvoiceByIDParams, userContext auth.Context, loggerContext *logrus.Entry) middleware.Responder {
	c := s.createInvoicingClient(params.XRequestID, loggerContext)

	var userInfo *payment_processing.UserInfo
	var invoiceID domain.InvoiceID
	var invoiceState *payment_processing.InvoiceState

	userInfo = getUserInfo(userContext)
	invoiceID = encodeInvoiceID(params.InvoiceID)
	invoiceState, err := c.Get(userInfo, invoiceID)
	if err != nil {
		loggerContext.Error("Wrong response: ", err.Error())
		panic(err)
	}

	incomingInvoice := invoiceState.Invoice
	payload := &models.Invoice{
		Amount:      decodeAmount(incomingInvoice.Cost.Amount),
		Context:     decodeInvoiceContext(incomingInvoice.Context),
		Currency:    decodeCurrency(incomingInvoice.Cost.Currency.SymbolicCode),
		Description: decodeDescription(*incomingInvoice.Description),
		DueDate:     decodeDateTime(incomingInvoice.Due),
		ID:          decodeInvoiceID(incomingInvoice.ID),
		Product:     decodeProduct(incomingInvoice.Product),
		Status:      decodeInvoiceStatus(incomingInvoice.Status),
	}

	loggerContext.Info("success")
	return invoices.NewGetInvoiceByIDOK().WithPayload(payload)
}

func (s *realCommonAPIService) GetInvoiceEvents(params invoices.GetInvoiceEventsParams, userContext auth.Context, loggerContext *logrus.Entry) middleware.Responder {
	c := s.createInvoicingClient(params.XRequestID, loggerContext)

	var userInfo *payment_processing.UserInfo
	var invoiceID domain.InvoiceID
	var eventRange payment_processing.EventRange
	var incomingEvents payment_processing.Events

	userInfo = getUserInfo(userContext)

	invoiceID = encodeInvoiceID(params.InvoiceID)
	eventRange = encodeEventRange(params.Limit, params.EventID)

	incomingEvents, err := c.GetEvents(userInfo, invoiceID, &eventRange)
	if err != nil {
		loggerContext.Error("Wrong response: ", err.Error())
		panic(err)
	}

	payload := decodeEvents(incomingEvents)
	loggerContext.Info("success")
	return invoices.NewGetInvoiceEventsOK().WithPayload(payload)
}

func (s *realCommonAPIService) CreatePaymentToolToken(params tokens.CreatePaymentToolTokenParams, userContext auth.Context, loggerContext *logrus.Entry) middleware.Responder {
	paymentTool := params.PaymentTool

	switch paymentTool.PaymentToolType() {
	case "CardData":
		cardData := paymentTool.(*models.CardData)
		expDate, _ := parseExpDate(cardData.ExpDate)
		cdsCardData := &cds.CardData{
			Pan:            *cardData.CardNumber,
			ExpDate:        expDate,
			CardholderName: cardData.CardHolder,
			Cvv:            *cardData.Cvv,
		}
		c := s.createStorageClient(params.XRequestID, loggerContext)

		var putResult *cds.PutCardDataResult_
		putResult, err := c.PutCardData(cdsCardData)
		if err != nil {
			loggerContext.Error("Wrong response: ", err.Error())
			panic(err)
		}

		var token models.PaymentToolToken
		token = s.bankCardToToken(putResult.BankCard)
		session := decodePaymentSession(putResult.Session)

		payload := tokens.CreatePaymentToolTokenCreatedBodyBody{
			Token:   token,
			Session: session,
		}
		loggerContext.Info("success")
		return tokens.NewCreatePaymentToolTokenCreated().WithPayload(payload)
	default:
		return middleware.NotImplemented("Not Implemented")
	}
}

func (s *realCommonAPIService) CreatePayment(params payments.CreatePaymentParams, userContext auth.Context, loggerContext *logrus.Entry) middleware.Responder {
	c := s.createInvoicingClient(params.XRequestID, loggerContext)
	incomingArgs := params.CreatePaymentArgs
	var userInfo *payment_processing.UserInfo
	var invoiceID domain.InvoiceID
	var invoicePaymentParams *payment_processing.InvoicePaymentParams
	var paymentID domain.InvoicePaymentID
	var payer *domain.Payer
	var paymentTool domain.PaymentTool
	var paymentSession domain.PaymentSession

	userInfo = getUserInfo(userContext)
	invoiceID = encodeInvoiceID(params.InvoiceID)
	paymentSession = encodePaymentSession(incomingArgs.PaymentSession)

	paymentTool = s.encodePaymentTool(incomingArgs.PaymentToolToken)

	invoicePaymentParams = &payment_processing.InvoicePaymentParams{
		Payer:       payer,
		PaymentTool: &paymentTool,
		Session:     paymentSession,
	}
	paymentID, err := c.StartPayment(userInfo, invoiceID, invoicePaymentParams)
	if err != nil {
		loggerContext.Error("Wrong response: ", err.Error())
		panic(err)
	}

	id := decodeInvoicePaymentID(paymentID)
	payload := payments.CreatePaymentCreatedBodyBody{
		ID: &id,
	}
	loggerContext.Info("success")
	return payments.NewCreatePaymentCreated().WithPayload(payload)
}

func (s *realCommonAPIService) GetPaymentByID(params payments.GetPaymentByIDParams, userContext auth.Context, loggerContext *logrus.Entry) middleware.Responder {
	c := s.createInvoicingClient(params.XRequestID, loggerContext)

	var userInfo *payment_processing.UserInfo
	var domainPaymentID domain.InvoicePaymentID
	var domainPayment *domain.InvoicePayment

	userInfo = getUserInfo(userContext)

	domainPaymentID = encodeInvoicePaymentID(params.PaymentID)
	domainPayment, err := c.GetPayment(userInfo, domainPaymentID)
	if err != nil {
		loggerContext.Error("Wrong response: ", err.Error())
		panic(err)
	}

	createdAt := decodeDateTime(domainPayment.CreatedAt)
	paymentID := decodeInvoicePaymentID(domainPayment.ID)
	paymentToolToken := s.decodePaymentTool(*domainPayment.PaymentTool)
	status := decodeInvoicePaymentStatus(domainPayment.Status)
	invoiceID := params.InvoiceID

	payload := &models.Payment{
		CreatedAt:        &createdAt,
		ID:               &paymentID,
		InvoiceID:        &invoiceID,
		PaymentToolToken: paymentToolToken,
		Status:           &status,
	}

	return payments.NewGetPaymentByIDOK().WithPayload(payload)
	return middleware.NotImplemented("Not Implemented")
}

type EventHandler struct {
	loggerContext *logrus.Entry
}

func (e EventHandler) HandleCallService(id event.RpcID, meta event.MetaCallService) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "call service",
	}).Debug()
}

func (e EventHandler) HandleClientSend(id event.RpcID, meta event.MetaClientSend) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "client send",
	}).Debug()
}

func (e EventHandler) HandleClientReceive(id event.RpcID, meta event.MetaClientReceive) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "client receive",
	}).Debug()
}

func (e EventHandler) HandleServiceResult(id event.RpcID, meta event.MetaServiceResult) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "service result",
	}).Debug()
}

func (e EventHandler) HandleServerReceive(id event.RpcID, meta event.MetaServerReceive) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "server receive",
	}).Debug()
}

func (e EventHandler) HandleCallServiceHandler(id event.RpcID, meta event.MetaCallServiceHandler) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "call service handler",
	}).Debug()
}

func (e EventHandler) HandleServiceHandlerResult(id event.RpcID, meta event.MetaServiceHandlerResult) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "service handler result",
	}).Debug()
}

func (e EventHandler) HandleServerSend(id event.RpcID, meta event.MetaServerSend) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "server send",
	}).Debug()
}

func (e EventHandler) HandleThriftError(id event.RpcID, meta event.MetaThriftError) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "thrift error",
	}).Debug()
}

func (e EventHandler) HandleInternalError(id event.RpcID, meta event.MetaInternalError) {
	e.loggerContext.WithFields(logrus.Fields{
		"span_id":   id.SpanID,
		"parent_id": id.ParentID,
		"trace_id":  id.TraceID,
		"meta":      meta,
		"event":     "internal error",
	}).Debug()
}

func parseExpDate(raw *string) (*cds.ExpDate, error) {
	parsed := strings.Split(*raw, "/")
	expDate := new(cds.ExpDate)
	month, err := strconv.Atoi(parsed[0])
	if err != nil {
		return expDate, err
	}
	year, err := strconv.Atoi(parsed[1])
	if err != nil {
		return expDate, err
	}
	expDate.Month = int8(month)
	expDate.Year = int16(2000 + year)
	return expDate, nil
}

func (s *realCommonAPIService) createStorageClient(xRequestID string, loggerContext *logrus.Entry) cds.Storage {
	c := context.NewContext(xRequestID, s.getEventHandler(loggerContext))
	myClient := client.NewHttpClient(s.config.Cds.Url, c)
	return cds.NewStorageClientFactory(myClient.Transport(), myClient.ProtocolFactory())
}

func (s *realCommonAPIService) createInvoicingClient(xRequestID string, loggerContext *logrus.Entry) payment_processing.Invoicing {
	c := context.NewContext(xRequestID, s.getEventHandler(loggerContext))
	myClient := client.NewHttpClient(s.config.Processing.Url, c)
	return payment_processing.NewInvoicingClientFactory(myClient.Transport(), myClient.ProtocolFactory())
}

func (s *realCommonAPIService) getEventHandler(loggerContext *logrus.Entry) event.Handler {
	return EventHandler{
		loggerContext: loggerContext,
	}
}

func (s *realCommonAPIService) encodePaymentTool(token models.PaymentToolToken) domain.PaymentTool {
	bankCard := s.decodeBankCard(string(token))
	return domain.PaymentTool{
		BankCard: bankCard,
	}
}

func (s *realCommonAPIService) decodePaymentTool(paymentTool domain.PaymentTool) models.PaymentToolToken {
	return s.bankCardToToken(paymentTool.BankCard)
}

func (s *realCommonAPIService) bankCardToToken(bankCard *domain.BankCard) models.PaymentToolToken {
	return models.PaymentToolToken(s.encodeBankCard(bankCard))
}

func (s *realCommonAPIService) encodeBankCard(bankCard *domain.BankCard) string {
	preparedbankCard, err := json.Marshal(&bankCard)
	if err != nil {
		panic(err)
	}
	data := []byte(preparedbankCard)
	return base64.StdEncoding.EncodeToString(data)
}

func (s *realCommonAPIService) decodeBankCard(token string) *domain.BankCard {
	decoded, err := base64.StdEncoding.DecodeString(token)
	if err != nil {
		panic(err)
	}

	bankCard := new(domain.BankCard)
	err = json.Unmarshal(decoded, bankCard)
	if err != nil {
		panic(err)
	}
	return bankCard
}

func getUserInfo(userContext auth.Context) *payment_processing.UserInfo {
	userID := (userContext["sub"]).(string) //@FIXME avoid panic in case of empty field
	userInfo := new(payment_processing.UserInfo)
	userInfo.ID = payment_processing.UserID(base.ID(userID))
	return userInfo
}

//// Hello shitcode! Hello Artemiy!
func convertDueDate(dueDate string) base.Timestamp {
	parsedTime, err := time.Parse("2006-01-02T15:04:05Z", dueDate)
	if err != nil {
		panic(err)
	}
	return base.Timestamp(parsedTime.String())
}

func convertContext(context models.InvoiceContext) domain.InvoiceContext {
	return domain.InvoiceContext([]byte{})
}

func encodeInvoiceID(id string) domain.InvoiceID {
	return domain.InvoiceID(id)
}

func decodeInvoiceID(id domain.InvoiceID) string {
	return string(id)
}
func encodeInvoicePaymentID(id string) domain.InvoicePaymentID {
	return domain.InvoicePaymentID(id)
}

func decodeInvoicePaymentID(id domain.InvoicePaymentID) string {
	return string(id)
}

func encodeAmount(amount models.Amount) domain.Amount {
	return domain.Amount(int64(amount))
}

func decodeAmount(amount domain.Amount) models.Amount {
	return models.Amount(int64(amount))
}

func encodeCurrency(currency models.Currency) domain.CurrencyRef {
	return domain.CurrencyRef{
		SymbolicCode: string(currency),
	}
}

func decodeCurrency(currency string) models.Currency {
	return models.Currency(currency)
}

func encodePaymentSession(session models.PaymentSession) domain.PaymentSession {
	return domain.PaymentSession(string(session))
}

func decodePaymentSession(session domain.PaymentSession) models.PaymentSession {
	return models.PaymentSession(string(session))
}

func encodeEventRange(limit int64, eventID *string) payment_processing.EventRange {
	after := (*string)(eventID)
	return payment_processing.EventRange{
		After: (*payment_processing.EventID)(after),
		Limit: int32(limit),
	}
}

func decodeEvents(events payment_processing.Events) []*models.Event {
	var result []*models.Event
	for _, event := range events {
		id := event.ID
		eventType := event.Ev

		preparedID := string(id)
		item := &models.Event{
			ID:        &preparedID,
			EventType: eventType,
		}
		result = append(result, item)
	}
	return result
}

func encodeProduct(product models.Product) string {
	return string(product)
}

func decodeProduct(product string) models.Product {
	return models.Product(product)
}

func encodeDescription(description models.InvoiceDescription) string {
	return string(description)
}

func decodeDescription(description string) models.InvoiceDescription {
	return models.InvoiceDescription(description)
}

func encodeDateTime(dueDate string) base.Timestamp {
	return base.Timestamp(dueDate) //a placeholder for validation
}

func decodeDateTime(dueDate base.Timestamp) string {
	return string(dueDate) //a placeholder for validation
}

func encodeInvoiceContext(context models.InvoiceContext) domain.InvoiceContext {
	c := context.(interface{})
	encoded, _ := json.Marshal(&c)
	return domain.InvoiceContext(encoded)
}

func decodeInvoiceContext(context domain.InvoiceContext) models.InvoiceContext {
	c := []byte(context)
	var decoded interface{}
	json.Unmarshal(c, &decoded)
	return models.InvoiceContext(decoded)
}

func decodeInvoiceStatus(status domain.InvoiceStatus) string {
	return status.String()
}

func decodeInvoicePaymentStatus(status domain.InvoicePaymentStatus) string {
	return status.String()
}
